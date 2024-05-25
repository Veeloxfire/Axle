#include <AxleUtil/format.h>
#include <AxleUtil/io.h>

#include "compiler.h"
#include "type.h"
#include "ast.h"
#include "parser.h"
#include "operators.h"
#include "backends.h"
#include "ir.h"
#include "type_check.h"

#ifdef AXLE_TRACING
#include <Tracer/trace.h>
#endif

namespace IO = Axle::IO;
namespace Format = Axle::Format;

CompilationUnit* CompilationUnitStore::allocate_unit() {
  CompilationUnit* unit = compilation_units.allocate();

  unit->id = ++comp_unit_counter;

  active_units.insert(unit);

  return unit;
}

void CompilationUnitStore::free_unit(CompilationUnit* unit) {
  active_units.remove_if([id = unit->id](CompilationUnit* u) { return u->id == id; });

  compilation_units.free(unit);
}

CompilationUnit* CompilationUnitStore::get_unit_if_exists(u64 id) const {
  FOR(active_units, it) {
    CompilationUnit* u = *it;
    if (u->id == id) {
      return u;
    }
  }

  return nullptr;
}

IR::GlobalLabel CompilerGlobals::next_function_label(const SignatureStructure* s, const Span& span) {
  label_mutex.acquire();
  IR::GlobalLabel label = { label_signature_table.size + 1 };
  label_signature_table.insert({s, span});
  label_mutex.release();

  return label;
}

GlobalLabelInfo CompilerGlobals::get_label_info(IR::GlobalLabel label) {
  label_mutex.acquire();
  GlobalLabelInfo info = label_signature_table[label.label - 1];
  label_mutex.release();

  return info;
}

IR::IRStore* CompilerGlobals::new_ir(IR::GlobalLabel label, const SignatureStructure* sig) {
  ir_mutex.acquire();
  IR::IRStore* ir = ir_builders_single_threaded.insert();
  ir_mutex.release();

  ir->global_label = label;
  ir->signature = sig;

  return ir;
}

IR::Function* CompilerGlobals::new_function() {
  functions_mutex.acquire();
  IR::Function* func = functions_single_threaded.insert();
  functions_mutex.release();
  return func;
}

Local* CompilerGlobals::new_local() {
  locals_mutex.acquire();
  Local* loc = locals_single_threaded.insert();
  locals_mutex.release();
  return loc;
}

Global* CompilerGlobals::new_global() {
  globals_mutex.acquire();
  Global* glob = globals_single_threaded.insert();
  globals_mutex.release();
  return glob;
}

Namespace* CompilerGlobals::new_namespace() {
  namespaces_mutex.acquire();
  Namespace* names = namespaces_single_threaded.insert();
  namespaces_mutex.release();

  return names;
}

CompilationUnit* new_compilation_unit(Compilation* const comp,
                                      COMPILATION_EMIT_TYPE emit_type,
                                      Pipe* main_pipe,
                                      Namespace* names,
                                      AST_LOCAL root,
                                      void* detail,
                                      bool print) {
  ASSERT(comp != nullptr);
  ASSERT(names != nullptr);
  ASSERT(detail != nullptr);
  ASSERT(main_pipe != nullptr);

  CompilationUnit* unit = comp->store.allocate_unit();
  unit->unit_wait_on_count = 0;
  unit->unfound_wait_on_count = 0;
  unit->dependency_list = nullptr;
  unit->main_pipe = main_pipe;

  unit->emit = emit_type;
  unit->available_names = names;
  unit->ast = root;
  unit->detail = detail;

  comp->dependencies.in_flight_units += 1;

  if (print) {
    IO::format("Started Comp Unit {}       | Active = {}, In flight = {}\n",
                 unit->id, comp->store.active_units.size, comp->dependencies.in_flight_units);
  }


  return unit;
}

void set_dependency(CompilerThread* const comp_thread, UnitID id) {
  ASSERT(id != NULL_ID);

  comp_thread->new_depends.insert(id);
}

template<typename ... T>
void set_unfound_name(CompilerThread* const comp_thread,
                      UnknownName&& name,
                      ERROR_CODE code, const Span& span,
                      const Format::FormatString& f_message, T&& ... ts) {

  ASSERT(name.ident != nullptr);

  comp_thread->local_unfound_names.names.insert_uninit(1);
  UnfoundNameHolder* dep = comp_thread->local_unfound_names.names.back();

  dep->name = std::move(name);
  dep->dependency = nullptr;
  dep->as_error.type = code;
  dep->as_error.span = span;
  dep->as_error.message = format(f_message, std::forward<T>(ts)...);
}

Local* DependencyChecker::get_local(const Axle::InternString* name) {
  auto i = locals.begin();
  const auto end = locals.end();

  for (; i < end; i++) {
    Local* l = *i;

    if (l->decl.name == name) {
      return l;
    }
  }

  return nullptr;
}

static u32 new_dynamic_init_object(CompilerGlobals* const comp, u32 size, u32 alignment,
                                   IR::IRStore* ir) {
  Backend::GlobalData holder = {};
  holder.size = size;
  holder.alignment = alignment;
  holder.constant_init = false;
  holder.init_expr_label = ir->global_label;

  comp->dynamic_inits.insert(holder);
  return (u32)comp->dynamic_inits.size;
}

static u32 new_dynamic_init_object_const(CompilerGlobals* const comp, u32 size, u32 alignment,
                                         const u8* value) {
  Backend::GlobalData holder = {};
  holder.size = size;
  holder.alignment = alignment;
  holder.constant_init = true;
  holder.constant_value = value;

  comp->dynamic_inits.insert(holder);
  return (u32)comp->dynamic_inits.size;
}

Global* test_global_dependency(CompilerGlobals* const comp, CompilerThread* const comp_thread, DependencyChecker* const state, const Span& span, const Axle::InternString* ident) {
  auto names = comp->services.names.get();
  const GlobalName* name = names->find_global_name(state->available_names, ident);

  if (name == nullptr) {
    names.release();
    UnknownName unknown = {};
    unknown.ident = ident;
    unknown.ns = state->available_names;

    set_unfound_name(comp_thread, std::move(unknown),
                     ERROR_CODE::NAME_ERROR, span,
                     "Could not find name '{}'", ident);
    return nullptr;
  }
  else {
    Global* g = name->global;
    names.release();
    ASSERT(name->global->decl.type.is_valid());
    return g;
  }
}

void dependency_check_ast_node(CompilerGlobals* const comp,
                               CompilerThread* const comp_thread,
                               DependencyChecker* const state,
                               AST_LOCAL a) {
  TELEMETRY_FUNCTION();

  ASSERT(a != nullptr);

  switch (a->ast_type) {
    case AST_TYPE::INVALID: INVALID_CODE_PATH("Invalid node type"); break;
    case AST_TYPE::NAMED_TYPE: {
        ASTNamedType* nt = (ASTNamedType*)a;

        nt->global = test_global_dependency(comp, comp_thread, state, nt->node_span, nt->name);
        return;
      }
    case AST_TYPE::ARRAY_TYPE: {
        ASTArrayType* at = (ASTArrayType*)a;

        dependency_check_ast_node(comp, comp_thread, state, at->base);
        dependency_check_ast_node(comp, comp_thread, state, at->expr);
        return;
      }
    case AST_TYPE::PTR_TYPE: {
        ASTPtrType* ptr = (ASTPtrType*)a;

        dependency_check_ast_node(comp, comp_thread, state, ptr->base);

        return;
      }
    case AST_TYPE::SLICE_TYPE: {
        ASTSliceType* ptr = (ASTSliceType*)a;

        dependency_check_ast_node(comp, comp_thread, state, ptr->base);

        return;
      }
    case AST_TYPE::LAMBDA_TYPE: {
        ASTLambdaType* lt = (ASTLambdaType*)a;

        FOR_AST(lt->args, ty) {
          dependency_check_ast_node(comp, comp_thread, state, ty);
        }

        dependency_check_ast_node(comp, comp_thread, state, lt->ret);

        return;
      }
    case AST_TYPE::TUPLE_TYPE: {
        ASTTupleType* tt = (ASTTupleType*)a;

        FOR_AST(tt->types, ty) {
          dependency_check_ast_node(comp, comp_thread, state, ty);
        }

        return;
      }
    case AST_TYPE::CAST: {
        ASTCastExpr* cast = (ASTCastExpr*)a;

        dependency_check_ast_node(comp, comp_thread, state, cast->type);
        dependency_check_ast_node(comp, comp_thread, state, cast->expr);
        return;
      }
    case AST_TYPE::UNARY_OPERATOR: {
        ASTUnaryOperatorExpr* un_op = (ASTUnaryOperatorExpr*)a;

        dependency_check_ast_node(comp, comp_thread, state, un_op->expr);
        return;
      }
    case AST_TYPE::BINARY_OPERATOR: {
        ASTBinaryOperatorExpr* const bin_op = (ASTBinaryOperatorExpr*)a;

        dependency_check_ast_node(comp, comp_thread, state, bin_op->left);
        dependency_check_ast_node(comp, comp_thread, state, bin_op->right);
        return;
      }
    case AST_TYPE::IDENTIFIER_EXPR: {
        ASTIdentifier* ident = (ASTIdentifier*)a;

        const Axle::InternString* name = ident->name;

        {
          Local* local = state->get_local(name);

          if (local != nullptr) {
            ident->id_type = ASTIdentifier::LOCAL;
            ident->local = local;

            return;
          }
        }

        ident->id_type = ASTIdentifier::GLOBAL;//is definitely a global
        ident->global = test_global_dependency(comp, comp_thread, state, ident->node_span, ident->name);
        return;
      }
    case AST_TYPE::FUNCTION_CALL: {
        ASTFunctionCallExpr* const call = (ASTFunctionCallExpr*)a;

        dependency_check_ast_node(comp, comp_thread, state, call->function);

        FOR_AST(call->arguments, it) {
          dependency_check_ast_node(comp, comp_thread, state, it);
        }

        return;
      }
    case AST_TYPE::TUPLE_LIT: {
        ASTTupleLitExpr* tup = (ASTTupleLitExpr*)a;

        if (tup->prefix != nullptr) {
          dependency_check_ast_node(comp, comp_thread, state, tup->prefix);
        }

        FOR_AST(tup->elements, it) {
          dependency_check_ast_node(comp, comp_thread, state, it);
        }
        return;
      }
    case AST_TYPE::ARRAY_EXPR: {
        ASTArrayExpr* arr = (ASTArrayExpr*)a;

        FOR_AST(arr->elements, it) {
          dependency_check_ast_node(comp, comp_thread, state, it);
        }
        return;
      }
    case AST_TYPE::INDEX_EXPR: {
        ASTIndexExpr* index = (ASTIndexExpr*)a;

        dependency_check_ast_node(comp, comp_thread, state, index->expr);
        FOR_AST(index->arguments, it) {
          dependency_check_ast_node(comp, comp_thread, state, it);
        }
        return;
      }
    case AST_TYPE::MEMBER_ACCESS: {
        ASTMemberAccessExpr* member = (ASTMemberAccessExpr*)a;

        dependency_check_ast_node(comp, comp_thread, state, member->expr);
        return;
      }
    case AST_TYPE::LAMBDA_EXPR: {
        ASTLambdaExpr* le = (ASTLambdaExpr*)a;

        ASTLambda* lambda = (ASTLambda*)le->lambda;
        ASTFuncSig* sig = (ASTFuncSig*)lambda->sig;

        if (sig->sig->sig_struct == nullptr) {
          set_dependency(comp_thread, lambda->function->sig_unit_id);
        }

        return;
      }
    case AST_TYPE::STRUCT_EXPR: {
        ASTStructExpr* se = (ASTStructExpr*)a;

        ASTStructBody* struct_body = (ASTStructBody*)se->struct_body;

        if (!(struct_body->actual_type.is_valid())) {
          //Is not valid so need to wait for that
          set_dependency(comp_thread, struct_body->unit_id);
        }

        return;
      }
    case AST_TYPE::DECL: {
        ASTDecl* decl = (ASTDecl*)a;

        if (decl->type_ast != 0) {
          dependency_check_ast_node(comp, comp_thread, state, decl->type_ast);
        }

        if (decl->expr != 0) {
          dependency_check_ast_node(comp, comp_thread, state, decl->expr);
        }

        if (decl->decl_type == ASTDecl::TYPE::LOCAL) {
          if (decl->local_ptr == nullptr) {
            const Local* shadowing = state->get_local(decl->name);

            if (shadowing != nullptr) {
              comp_thread->report_error(ERROR_CODE::NAME_ERROR, a->node_span,
                                        "Attempted to shadow the local variable '{}'",
                                        decl->name);
              return;
            }


            state->num_locals += 1;
            Local* loc = comp->new_local();
            decl->local_ptr = loc;

            loc->decl.name = decl->name;
            loc->decl.span = decl->node_span;
          }

          state->locals.insert(decl->local_ptr);
        }

        return;
      }
    case AST_TYPE::TYPED_NAME: {
        ASTTypedName* tn = (ASTTypedName*)a;

        if (tn->type != 0) {
          dependency_check_ast_node(comp, comp_thread, state, tn->type);
        }

        if (tn->local_ptr == nullptr) {
          const Local* shadowing = state->get_local(tn->name);

          if (shadowing != nullptr) {
            comp_thread->report_error(ERROR_CODE::NAME_ERROR, a->node_span,
                                      "Attempted to shadow the variable '{}'",
                                      tn->name);
            return;
          }

          Local* loc = comp->new_local();
          tn->local_ptr = loc;

          loc->decl.name = tn->name;
          loc->decl.span = tn->node_span;
        }

        state->locals.insert(tn->local_ptr);

        return;
      }
    case AST_TYPE::ASSIGN: {
        ASTAssign* assign = (ASTAssign*)a;
        dependency_check_ast_node(comp, comp_thread, state, assign->assign_to);
        dependency_check_ast_node(comp, comp_thread, state, assign->value);

        return;
      }
    case AST_TYPE::BLOCK: {
        ASTBlock* block = (ASTBlock*)a;

        const usize count = state->locals.size;

        FOR_AST(block->block, it) {
          dependency_check_ast_node(comp, comp_thread, state, it);
        }

        state->locals.pop_n(state->locals.size - count);

        return;
      }
    case AST_TYPE::IF_ELSE: {
        ASTIfElse* if_else = (ASTIfElse*)a;

        dependency_check_ast_node(comp, comp_thread, state, if_else->condition);

        const usize count = state->locals.size;

        dependency_check_ast_node(comp, comp_thread, state, if_else->if_statement);

        state->locals.pop_n(state->locals.size - count);

        if (if_else->else_statement != 0) {
          dependency_check_ast_node(comp, comp_thread, state, if_else->else_statement);
          state->locals.pop_n(state->locals.size - count);
        }

        return;
      }
    case AST_TYPE::WHILE: {
        ASTWhile* while_s = (ASTWhile*)a;

        dependency_check_ast_node(comp, comp_thread, state, while_s->condition);

        const usize count = state->locals.size;
        dependency_check_ast_node(comp, comp_thread, state, while_s->statement);
        state->locals.pop_n(state->locals.size - count);
        return;
      }
    case AST_TYPE::RETURN: {
        ASTReturn* ret = (ASTReturn*)a;

        if (ret->expr != nullptr) {
          dependency_check_ast_node(comp, comp_thread, state, ret->expr);
        }
        return;
      }
    case AST_TYPE::FUNCTION_SIGNATURE: {
        ASTFuncSig* func_sig = (ASTFuncSig*)a;

        FOR_AST(func_sig->parameters, it) {
          dependency_check_ast_node(comp, comp_thread, state, it);
        }

        dependency_check_ast_node(comp, comp_thread, state, func_sig->return_type);

        return;
      }
    case AST_TYPE::IMPORT: {
        ASTImport* imp = (ASTImport*)a;

        dependency_check_ast_node(comp, comp_thread, state, imp->expr_location);
        return;
      }
    case AST_TYPE::EXPORT_SINGLE: {
        ASTExportSingle* es = (ASTExportSingle*)a;

        dependency_check_ast_node(comp, comp_thread, state, es->value);
        return;
      }
    case AST_TYPE::EXPORT: {
        ASTExport* e = (ASTExport*)a;

        FOR_AST(e->export_list, it) {
          dependency_check_ast_node(comp, comp_thread, state, it);
        }
        return;
      }
    case AST_TYPE::LINK: {
        ASTLink* imp = (ASTLink*)a;

        dependency_check_ast_node(comp, comp_thread, state, imp->import_type);

        return;
      }

    case AST_TYPE::LAMBDA: {
        ASTLambda* l = (ASTLambda*)a;

        ASSERT(state->locals.size == 0);
        ASSERT(state->num_locals == 0);

        dependency_check_ast_node(comp, comp_thread, state, l->sig);
        dependency_check_ast_node(comp, comp_thread, state, l->body);

        return;
      }
    case AST_TYPE::STRUCT: {
        ASTStructBody* s = (ASTStructBody*)a;

        FOR_AST(s->elements, it) {
          dependency_check_ast_node(comp, comp_thread, state, it);
        }

        return;
      }

    case AST_TYPE::ASCII_CHAR:
    case AST_TYPE::ASCII_STRING:
    case AST_TYPE::NUMBER: {
        //No dependencies :)
        return;
      }
  }

  comp_thread->report_error(ERROR_CODE::INTERNAL_ERROR, a->node_span,
                            "Not yet implemented dependency checking for this node. Node ID: {}", (usize)a->ast_type);
}

struct NodeEval {
  AST_LOCAL expr;
  IR::ValueRequirements requirements;

  NodeEval forward(AST_LOCAL e2, IR::ValueRequirements req2 = {}) const {
    NodeEval n2 = {};
    n2.expr = e2;
    n2.requirements = requirements | req2;

    return n2;
  }

  static NodeEval new_value(AST_LOCAL e2, IR::ValueRequirements req2 = {}) {
    NodeEval n2 = {};
    n2.expr = e2;
    n2.requirements = req2;

    return n2;
  }
};

static Eval::RuntimeValue compile_bytecode(CompilerGlobals* const comp,
                                           CompilerThread* const comp_thread,
                                           Eval::IrBuilder* const builder,
                                           const NodeEval& eval);

static Eval::RuntimeValue compile_function_call(CompilerGlobals* const comp,
                                                CompilerThread* const comp_thread,
                                                Eval::IrBuilder* const builder,
                                                const NodeEval& eval) {
  TELEMETRY_FUNCTION();

  IR::IRStore* const ir = builder->ir;

  const ASTFunctionCallExpr* const call = downcast_ast<ASTFunctionCallExpr>(eval.expr);

  const auto* sig_struct = call->sig;

  bool has_return = sig_struct->return_type != comp_thread->builtin_types->t_void;

  Axle::Array<IR::V_ARG> args;
  args.reserve_total(call->arguments.count + (usize)has_return);

  FOR_AST(call->arguments, it) {
    Eval::RuntimeValue val = compile_bytecode(comp, comp_thread, builder, NodeEval::new_value(it));
    if (comp_thread->is_panic()) {
      return Eval::no_value();
    }

    IR::V_ARG v_arg = Eval::load_v_arg(ir, val);
    args.insert(v_arg);
  }

  ASSERT(args.size == call->arguments.count);


  if (has_return) {
    IR::ValueIndex v = ir->new_temporary(sig_struct->return_type, eval.requirements);

    args.insert(IR::v_arg(v, 0, sig_struct->return_type));
  }

  {
    IR::Types::Call c = {};
    c.label = call->label;
    c.n_values = (u32)args.size;
    c.values = args.data;

    IR::Emit::Call(ir->current_bytecode(), c);

    ir->current_control_block()->calls = true;
  }

  if (has_return) {
    const IR::V_ARG* sv = args.back();

    ASSERT(sv->offset == 0);
    ASSERT(sv->size == sig_struct->return_type.size());

    return Eval::as_direct(sv->val, sig_struct->return_type);
  }
  else {
    return Eval::no_value();
  }
}


Eval::RuntimeValue CASTS::int_to_int(IR::IRStore* const ir,
                                     const Type& to,
                                     const Eval::RuntimeValue& val) {
  ASSERT(to.is_valid() && to.struct_type() == STRUCTURE_TYPE::INTEGER);
  const Type from = val.effective_type();
  ASSERT(from.is_valid() && from.struct_type() == STRUCTURE_TYPE::INTEGER);

  if (to == from) {
    return val;
  }

  IR::ValueIndex temp = ir->new_temporary(to, {});

  IR::Types::Copy cc = {};
  cc.from = Eval::load_v_arg(ir, val);
  cc.to = IR::v_arg(temp, 0, to);

  IR::Emit::Copy(ir->current_bytecode(), cc);
  return Eval::as_direct(temp, to);
}

Eval::RuntimeValue CASTS::no_op(IR::IRStore* const,
                                const Type& to,
                                const Eval::RuntimeValue& val) {
  ASSERT(val.type.is_valid());
  Eval::RuntimeValue res = val;
  res.type = to;
  return res;
}

Eval::RuntimeValue CASTS::take_address(IR::IRStore* const ir,
                                       const Type& to,
                                       const Eval::RuntimeValue& val) {
  ASSERT(val.type.is_valid());
  return Eval::addrof(ir, val, to);
}

static Type generate_pointer_type(CompilerGlobals* comp, const Type& base) {
  const PointerStructure* ps;
  {
    Axle::AtomicLock<Structures> structures = {};
    Axle::AtomicLock<Axle::StringInterner> strings = {};
    comp->services.get_multiple(&structures, &strings);

    ps = find_or_make_pointer_structure(structures._ptr, strings._ptr, base);
  }

  return to_type(ps);
}

Eval::RuntimeValue load_data_memory(CompilerGlobals* comp, Eval::IrBuilder* builder, Global* global,
                                    const IR::ValueRequirements reqs) {
  TELEMETRY_FUNCTION();

  IR::IRStore* const ir = builder->ir;

  if (!global->is_runtime_available) {
    global->dynamic_init_index = new_dynamic_init_object_const(comp,
                                                               global->decl.type.size(),
                                                               global->decl.type.structure->alignment,
                                                               global->decl.init_value);
    global->is_runtime_available = true;
  }

  u32 global_id = 0;
  {
    Axle::ViewArr<IR::GlobalReference> grs = view_arr(ir->globals_used);
    const usize count = ir->globals_used.size;

    for (; global_id < count; ++global_id) {
      if (grs[global_id].data_member == global->dynamic_init_index) {
        grs[global_id].requirements |= reqs;
        goto FOUND;
      }
    }

    global_id = ir->new_global_reference({ global->decl.type, reqs, global->dynamic_init_index });
  }

FOUND:
  const Type ptr_type = generate_pointer_type(comp, global->decl.type);

  IR::ValueIndex v = ir->new_temporary(ptr_type, {});

  IR::Types::AddrOfGlobal args = {};
  args.val = IR::v_arg(v, 0, ptr_type);
  args.im32 = global_id;

  IR::Emit::AddrOfGlobal(ir->current_bytecode(), args);

  return Eval::as_indirect(v, ptr_type);
}

//Note: Recursive 
Eval::RuntimeValue compile_bytecode(CompilerGlobals* const comp,
                                    CompilerThread* const comp_thread,
                                    Eval::IrBuilder* const builder,
                                    const NodeEval& eval) {
  TELEMETRY_FUNCTION();

  AST_LOCAL expr = eval.expr;
  ASSERT(expr->node_type.is_valid());

  switch (expr->ast_type) {
    case AST_TYPE::NAMED_TYPE: {
        ASTNamedType* nt = (ASTNamedType*)expr;

        ASSERT(builder->eval_time == Eval::Time::CompileTime);
        ASSERT(!eval.requirements.has_address());

        Type* struct_c = comp->new_constant<Type>();
        Axle::memcpy_ts(struct_c, 1, &nt->actual_type, 1);

        return Eval::as_constant((const u8*)struct_c, comp_thread->builtin_types->t_type);
      }
    case AST_TYPE::ARRAY_TYPE: {
        ASTArrayType* nt = (ASTArrayType*)expr;

        ASSERT(builder->eval_time == Eval::Time::CompileTime);
        ASSERT(!eval.requirements.has_address());

        Type* struct_c = comp->new_constant<Type>();
        Axle::memcpy_ts(struct_c, 1, &nt->actual_type, 1);

        return Eval::as_constant((const u8*)struct_c, comp_thread->builtin_types->t_type);
      }
    case AST_TYPE::PTR_TYPE: {
        ASTPtrType* nt = (ASTPtrType*)expr;

        ASSERT(builder->eval_time == Eval::Time::CompileTime);
        ASSERT(!eval.requirements.has_address());

        Type* struct_c = comp->new_constant<Type>();
        Axle::memcpy_ts(struct_c, 1, &nt->actual_type, 1);

        return Eval::as_constant((const u8*)struct_c, comp_thread->builtin_types->t_type);
      }
    case AST_TYPE::SLICE_TYPE: {
        ASTSliceType* nt = (ASTSliceType*)expr;

        ASSERT(builder->eval_time == Eval::Time::CompileTime);
        ASSERT(!eval.requirements.has_address());

        Type* struct_c = comp->new_constant<Type>();
        Axle::memcpy_ts(struct_c, 1, &nt->actual_type, 1);

        return Eval::as_constant((const u8*)struct_c, comp_thread->builtin_types->t_type);
      }
    case AST_TYPE::LAMBDA_TYPE: {
        ASTLambdaType* nt = (ASTLambdaType*)expr;

        ASSERT(builder->eval_time == Eval::Time::CompileTime);
        ASSERT(!eval.requirements.has_address());

        Type* struct_c = comp->new_constant<Type>();
        Axle::memcpy_ts(struct_c, 1, &nt->actual_type, 1);

        return Eval::as_constant((const u8*)struct_c, comp_thread->builtin_types->t_type);
      }
    case AST_TYPE::TUPLE_TYPE: {
        ASTTupleType* nt = (ASTTupleType*)expr;

        ASSERT(builder->eval_time == Eval::Time::CompileTime);
        ASSERT(!eval.requirements.has_address());

        Type* struct_c = comp->new_constant<Type>();
        Axle::memcpy_ts(struct_c, 1, &nt->actual_type, 1);

        return Eval::as_constant((const u8*)struct_c, comp_thread->builtin_types->t_type);
      }
    case AST_TYPE::STRUCT_EXPR: {
        ASTStructExpr* se = (ASTStructExpr*)expr;
        ASTStructBody* s = (ASTStructBody*)se->struct_body;

        ASSERT(builder->eval_time == Eval::Time::CompileTime);
        ASSERT(!eval.requirements.has_address());

        Type* struct_c = comp->new_constant<Type>();
        Axle::memcpy_ts(struct_c, 1, &s->actual_type, 1);

        return Eval::as_constant((const u8*)struct_c, comp_thread->builtin_types->t_type);
      }
    case AST_TYPE::LAMBDA_EXPR: {
        ASTLambdaExpr* le = (ASTLambdaExpr*)expr;
        ASTLambda* l = (ASTLambda*)le->lambda;

        ASSERT(builder->eval_time == Eval::Time::CompileTime);
        ASSERT(!eval.requirements.has_address());

        IR::GlobalLabel* label = comp->new_constant<IR::GlobalLabel>();
        *label = l->function->signature.label;

        ASSERT(le->node_type.struct_type() == STRUCTURE_TYPE::LAMBDA);

        return Eval::as_constant((const u8*)label, le->node_type);
      }
    case AST_TYPE::MEMBER_ACCESS: {
        ASTMemberAccessExpr* member_e = (ASTMemberAccessExpr*)expr;
        AST_LOCAL m_base = member_e->expr;

        STRUCTURE_TYPE st = m_base->node_type.struct_type();
        if (st == STRUCTURE_TYPE::COMPOSITE) {
          Eval::RuntimeValue obj = compile_bytecode(comp, comp_thread,
                                                    builder, eval.forward(m_base));
          if (comp_thread->is_panic()) {
            return Eval::no_value();
          }

          const Type ptr_type = generate_pointer_type(comp, member_e->node_type);

          u64* data = comp->new_constant<u64>();
          u64 val = member_e->offset;
          Axle::memcpy_ts(data, 1, &val, 1);

          const auto c = Eval::as_constant((const u8*)data, comp->builtin_types->t_u64);

          return Eval::sub_object(builder->ir, obj, c, ptr_type);
        }
        else if (st == STRUCTURE_TYPE::FIXED_ARRAY) {
          if (member_e->name == comp->important_names.ptr) {
            const auto next_eval = eval.forward(m_base, IR::ValueRequirements::Address);

            Eval::RuntimeValue obj = compile_bytecode(comp, comp_thread,
                                                      builder, next_eval);
            if (comp_thread->is_panic()) {
              return Eval::no_value();
            }

            Type ptr = member_e->node_type;
            ASSERT(ptr.struct_type() == STRUCTURE_TYPE::POINTER);

            return Eval::arr_to_ptr(builder->ir, obj, ptr);
          }
          else if (member_e->name == comp->important_names.len) {
            ASSERT(!eval.requirements.has_address());
            const ArrayStructure* as = m_base->node_type.unchecked_base<ArrayStructure>();

            u64 val = as->length;

            const size_t size = sizeof(val);

            uint8_t* val_c = comp->new_constant(size);
            Axle::memcpy_ts(val_c, size, (uint8_t*)&val, size);

            return Eval::as_constant(val_c, comp_thread->builtin_types->t_u64);
          }
          else {
            comp_thread->report_error(ERROR_CODE::INTERNAL_ERROR, expr->node_span,
                                      "No semantics supported for the member \"{}\" on an array", member_e->name);
            return Eval::no_value();
          }
        }
        else if (st == STRUCTURE_TYPE::SLICE) {
          comp_thread->report_error(ERROR_CODE::INTERNAL_ERROR, expr->node_span,
                                    "Slices not implemented yet");
          return Eval::no_value();
        }
        else {
          comp_thread->report_error(ERROR_CODE::INTERNAL_ERROR, expr->node_span,
                                    "Type \"{}\" does not support member access", m_base->node_type.name);
          return Eval::no_value();
        }
      }
    case AST_TYPE::INDEX_EXPR: {
        const Type base_type = expr->node_type;
        const size_t base_size = base_type.size();

        ASTIndexExpr* index = (ASTIndexExpr*)expr;
        AST_LOCAL index_expr = index->expr;
        const usize count = index->arguments.count;

        if(count == 1) {
          ASSERT(TYPE_TESTS::can_index(index_expr->node_type));

          AST_LOCAL index_index = index->arguments.start->curr;

          Eval::RuntimeValue arr = compile_bytecode(comp, comp_thread,
              builder, eval.forward(index_expr, IR::ValueRequirements::Address));
          if (comp_thread->is_panic()) {
            return Eval::no_value();
          }

          Eval::RuntimeValue index_val = compile_bytecode(comp, comp_thread,
              builder, NodeEval::new_value(index_index));
          if (comp_thread->is_panic()) {
            return Eval::no_value();
          }

          u64 u = base_size;
          const auto c = Eval::as_constant((const u8*)&u, comp_thread->builtin_types->t_u64);

          BinOpEmitInfo emit_info = {};
          {
            emit_info.dest_type = comp_thread->builtin_types->t_u64;
            emit_info.main_side = MainSide::LEFT;
            emit_info.func = &BinOpArgs::emit_mul_ints;
          }

          BinOpArgs args = {
            &emit_info,
            comp,
            builder->ir,
            index_val,
            c,
          };

          const Eval::RuntimeValue offset = args.emit_mul_ints();

          const Type t = generate_pointer_type(comp, base_type);

          return Eval::sub_object(builder->ir, arr, offset, t);
        }
        else if(count == 0 || count == 2) {
          comp_thread->report_error(ERROR_CODE::INTERNAL_ERROR, expr->node_span,
              "Slices not implemented yet");
          return Eval::no_value();
        }
        else {
          comp_thread->report_error(ERROR_CODE::INTERNAL_ERROR, expr->node_span,
              "Invalid number of index parameters: {}", count);
          return Eval::no_value();
        }
      }
    case AST_TYPE::TUPLE_LIT: {
        ASSERT(expr->node_type.struct_type() == STRUCTURE_TYPE::COMPOSITE);

        const auto* cpst = expr->node_type.unchecked_base<CompositeStructure>();

        ASSERT(!eval.requirements.has_address());
        ASTTupleLitExpr* lit = (ASTTupleLitExpr*)expr;

        Eval::RuntimeValue tup_lit = {};
        u8* tup_constant = nullptr;


        const bool is_constant = VC::is_comptime(expr->value_category);
        if (is_constant) {
          tup_constant = comp->new_constant(cpst->size);
          tup_lit = Eval::as_constant(tup_constant, expr->node_type);
        }
        else {
          IR::ValueIndex v = builder->ir->new_temporary(expr->node_type, eval.requirements);
          tup_lit = Eval::as_direct(v, expr->node_type);
        }

        auto i_t = cpst->elements.begin();

        FOR_AST(lit->elements, it) {
          Eval::RuntimeValue v = compile_bytecode(comp, comp_thread, builder, NodeEval::new_value(it));
          if (comp_thread->is_panic()) {
            return Eval::no_value();
          }

          if (is_constant) {
            ASSERT(tup_lit.rvt == Eval::RVT::Constant);
            ASSERT(v.rvt == Eval::RVT::Constant);
            const Type v_type = v.effective_type();
            ASSERT(v_type == i_t->type);
            memcpy_s(tup_constant + i_t->offset, i_t->type.size(), v.constant, v_type.size());
          }
          else {
            ASSERT(tup_lit.rvt == Eval::RVT::Direct);

            Eval::RuntimeValue member = tup_lit;
            member.value.offset = i_t->offset;
            member.type = i_t->type;

            Eval::assign(builder->ir, member, v);
          }

          ++i_t;
        }
        return tup_lit;
      }
    case AST_TYPE::ARRAY_EXPR: {
        const auto* const arr_type = expr->node_type.unchecked_base<ArrayStructure>();

        const Type base_type = arr_type->base;

        ASTArrayExpr* arr_expr = (ASTArrayExpr*)expr;

        ASSERT(!eval.requirements.has_address());

        Eval::RuntimeValue arr = {};
        u8* arr_constant = nullptr;

        const bool is_constant = VC::is_comptime(expr->value_category);
        if (is_constant) {
          arr_constant = comp->new_constant(arr_type->size);
          arr = Eval::as_constant(arr_constant, expr->node_type);
        }
        else {
          IR::ValueIndex v = builder->ir->new_temporary(expr->node_type, eval.requirements);
          arr = Eval::as_direct(v, expr->node_type);
        }

        usize count = 0;

        FOR_AST(arr_expr->elements, it) {
          Eval::RuntimeValue el = compile_bytecode(comp, comp_thread, builder, NodeEval::new_value(it));
          if (comp_thread->is_panic()) {
            return Eval::no_value();
          }

          const Type el_type = el.effective_type();

          ASSERT(el_type == base_type);
          u64 offset = count * base_type.size();

          if (is_constant) {
            ASSERT(el.rvt == Eval::RVT::Constant);
            ASSERT(arr.rvt == Eval::RVT::Constant);
            ASSERT(el_type == arr_type->base);
            memcpy_s(arr_constant + offset, base_type.size(), el.constant, base_type.size());
          }
          else {
            ASSERT(arr.rvt == Eval::RVT::Direct);

            Eval::RuntimeValue element_ref = arr;
            element_ref.value.offset = (u32)offset;
            element_ref.type = base_type;

            Eval::assign(builder->ir, element_ref, el);
          }

          count += 1;
        }

        return arr;
      }
    case AST_TYPE::ASCII_CHAR: {
        ASTAsciiChar* ch = (ASTAsciiChar*)expr;
        ASSERT(!eval.requirements.has_address());

        char* char_c = comp->new_constant<char>();
        *char_c = ch->character;

        return Eval::as_constant((const u8*)char_c, ch->node_type);
      }
    case AST_TYPE::ASCII_STRING: {
        ASTAsciiString* st = (ASTAsciiString*)expr;
        ASSERT(!eval.requirements.has_address());

        const auto* const arr_type = expr->node_type.unchecked_base<ArrayStructure>();

        const size_t size = arr_type->size;
        char* string_c = (char*)comp->new_constant(size);
        Axle::memcpy_ts(string_c, size, st->string->string, size);

        return Eval::as_constant((const u8*)string_c, st->node_type);
      }
    case AST_TYPE::NUMBER: {
        ASTNumber* num = (ASTNumber*)expr;
        ASSERT(!eval.requirements.has_address());

        const size_t size = expr->node_type.structure->size;

        uint8_t* val_c = comp->new_constant(size);
        Axle::memcpy_ts(val_c, size, (uint8_t*)&num->num_value, size);

        return Eval::as_constant(val_c, num->node_type);
      }
    case AST_TYPE::IDENTIFIER_EXPR: {
        ASTIdentifier* ident = (ASTIdentifier*)expr;

        if (ident->id_type == ASTIdentifier::LOCAL) {
          Local* local = ident->local;
          ASSERT(local != nullptr);

          builder->ir->variables[local->variable_id.variable].requirements |= eval.requirements;

          const Type t = local->decl.type;

          if (VC::is_comptime(local->decl.value_category) && !eval.requirements.has_address()) {
            ASSERT(local->decl.init_value != nullptr);
            return Eval::as_constant(local->decl.init_value, t);
          }
          else {
            return builder->import_variable(local->variable_id, eval.requirements);
          }
        }
        else if (ident->id_type == ASTIdentifier::GLOBAL) {
          Global* glob = ident->global;
          ASSERT(glob != nullptr);

          if (VC::is_comptime(glob->decl.value_category) && !eval.requirements.has_address()) {
            ASSERT(glob->decl.init_value != nullptr);
            return Eval::as_constant(glob->decl.init_value, glob->decl.type);
          }
          else {
            return load_data_memory(comp, builder, glob, eval.requirements);
          }
        }

        comp_thread->report_error(ERROR_CODE::INTERNAL_ERROR, expr->node_span,
                                  "Invalid or missing identifier type: {}", ident->id_type);
        return Eval::no_value();
      }
    case AST_TYPE::LINK: {
        const ASTLink* li = (ASTLink*)expr;
        ASSERT(!eval.requirements.has_address());

        const IR::DynLibraryImport& imp = comp->dyn_lib_imports[li->import_index - 1];

        IR::GlobalLabel* label_holder = comp->new_constant<IR::GlobalLabel>();
        *label_holder = imp.label;

        ASSERT(li->node_type.struct_type() == STRUCTURE_TYPE::LAMBDA);

        return Eval::as_constant((const u8*)label_holder, li->node_type);
      }
    case AST_TYPE::CAST: {
        const ASTCastExpr* const cast = (ASTCastExpr*)expr;
        ASSERT(!eval.requirements.has_address());

        Eval::RuntimeValue ref = compile_bytecode(comp, comp_thread,
                                                  builder, NodeEval::new_value(cast->expr));
        if (comp_thread->is_panic()) {
          return Eval::no_value();
        }

        const auto res = cast->emit(builder->ir, cast->node_type, ref);
        ASSERT(res.effective_type() == cast->node_type);
        return res;
      }
    case AST_TYPE::UNARY_OPERATOR: {
        const ASTUnaryOperatorExpr* const un_op = (ASTUnaryOperatorExpr*)expr;
        ASSERT(!eval.requirements.has_address());

        Eval::RuntimeValue ref;
        switch (un_op->op) {
          case UNARY_OPERATOR::NEG: {
              ref = compile_bytecode(comp, comp_thread,
                                     builder, NodeEval::new_value(un_op->expr));
              break;
            }
          case UNARY_OPERATOR::ADDRESS: {
              ref = compile_bytecode(comp, comp_thread,
                                     builder, NodeEval::new_value(un_op->expr, IR::ValueRequirements::Address));
              break;
            }
          case UNARY_OPERATOR::DEREF: {
              ref = compile_bytecode(comp, comp_thread,
                                     builder, NodeEval::new_value(un_op->expr));
              break;
            }
        }


        if (comp_thread->is_panic()) {
          return Eval::no_value();
        }

        UnOpArgs args = {
          &un_op->emit_info,
          comp,
          builder->ir,
          ref,
        };

        return args.emit();
      }
    case AST_TYPE::BINARY_OPERATOR: {
        const ASTBinaryOperatorExpr* const bin_op = (ASTBinaryOperatorExpr*)expr;
        AST_LOCAL left = bin_op->left;
        AST_LOCAL right = bin_op->right;

        Eval::RuntimeValue temp_left = compile_bytecode(comp, comp_thread,
                                                        builder, NodeEval::new_value(left));
        if (comp_thread->is_panic()) {
          return Eval::no_value();
        }

        Eval::RuntimeValue temp_right = compile_bytecode(comp, comp_thread,
                                                         builder, NodeEval::new_value(right));
        if (comp_thread->is_panic()) {
          return Eval::no_value();
        }

        BinOpArgs args = {
          &bin_op->emit_info,
          comp,
          builder->ir,
          temp_left,
          temp_right,
        };

        return args.emit();
      }
    case AST_TYPE::FUNCTION_CALL: {
        return compile_function_call(comp, comp_thread, builder, eval);
      }
    default: {
        //Invalid enum type
        //probably just didnt get around to supporting it
        comp_thread->report_error(ERROR_CODE::INTERNAL_ERROR, expr->node_span,
                                  "Invalid expression type: {}", ast_type_string(expr->ast_type));
        return Eval::no_value();
      }
  }
}

void compile_bytecode_of_statement(CompilerGlobals* const comp,
                                   CompilerThread* const comp_thread,
                                   Eval::IrBuilder* const builder,
                                   AST_LOCAL const statement) {
  TELEMETRY_FUNCTION();

  //TODD: warnings for inaccessible statements

  switch (statement->ast_type) {
    case AST_TYPE::ASSIGN: {
        ASTAssign* assign = (ASTAssign*)statement;

        Eval::RuntimeValue a = compile_bytecode(comp, comp_thread,
                                                builder, NodeEval::new_value(assign->assign_to));

        Eval::RuntimeValue v = compile_bytecode(comp, comp_thread,
                                                builder, NodeEval::new_value(assign->value));
        if (comp_thread->is_panic()) {
          return;
        }

        Eval::assign(builder->ir, a, v);
        return;
      }
    case AST_TYPE::BLOCK: {
        ASTBlock* block = (ASTBlock*)statement;

        usize scope_size = builder->variables_state.size;

        FOR_AST(block->block, it) {
          compile_bytecode_of_statement(comp, comp_thread, builder, it);
          if (comp_thread->is_panic()) {
            return;
          }
        }

        builder->rescope_variables(scope_size);

        return;
      }
    case AST_TYPE::RETURN: {
        ASTReturn* ret = (ASTReturn*)statement;

        if (ret->expr != nullptr) {
          Eval::RuntimeValue r = compile_bytecode(comp, comp_thread,
                                                  builder, NodeEval::new_value(ret->expr));
          if (comp_thread->is_panic()) {
            return;
          }

          const Type ret_type = builder->ir->signature->return_type;


          IR::ValueIndex ret_val = builder->ir->new_temporary(ret_type, {});
          Eval::assign(builder->ir, Eval::as_direct(ret_val, ret_type), r);

          builder->ir->set_current_cf(IR::CFReturn{
            builder->parent,
              ret_val,
          });
        }
        else {
          builder->ir->set_current_cf(IR::CFEnd{
            builder->parent,
          });
        }

        //Unreachable after this
        builder->ir->end_control_block();
        builder->ir->current_block = IR::NULL_LOCAL_LABEL;
        builder->parent = IR::NULL_LOCAL_LABEL;

        return;
      }
    case AST_TYPE::WHILE: {
        ASTWhile* const while_loop = (ASTWhile*)statement;

        IR::LocalLabel parent = builder->ir->current_block;

        IR::LocalLabel loop_merge_label = builder->ir->new_control_block();
        IR::LocalLabel loop_split_label = builder->ir->new_control_block();

        IR::LocalLabel body_label = builder->ir->new_control_block();
        IR::LocalLabel escape_label = builder->ir->new_control_block();

        builder->reset_variables();
        builder->ir->set_current_cf(IR::CFInline{
          builder->parent,
            loop_merge_label,
        });

        Axle::Array parent_variables = copy_arr(builder->variables_state);

        //Skip the merge part for now, do that later when we know what to merge

        IR::ValueIndex condition_vi;
        builder->switch_control_block(loop_split_label, loop_merge_label);
        {
          Eval::RuntimeValue cond = compile_bytecode(comp, comp_thread,
                                                     builder, NodeEval::new_value(while_loop->condition));
          if (comp_thread->is_panic()) {
            return;
          }

          condition_vi = builder->ir->new_temporary(comp_thread->builtin_types->t_bool, {});

          Eval::assign(builder->ir, Eval::as_direct(condition_vi, comp_thread->builtin_types->t_bool), cond);

          builder->ir->set_current_cf(IR::CFSplt{
            loop_merge_label,

              condition_vi,
              body_label,
              escape_label,
          });
        }
        builder->reset_variables();

        Axle::Array branch_variables = copy_arr(builder->variables_state);
        ASSERT(branch_variables.size == parent_variables.size);//Currently a waste of a copy, but might not be later on

        builder->switch_control_block(body_label, loop_split_label);
        {
          //loop branch
          compile_bytecode_of_statement(comp, comp_thread, builder, while_loop->statement);
          if (comp_thread->is_panic()) {
            return;
          }
        }
        IR::LocalLabel loop_end = builder->ir->current_block;

        if (loop_end == IR::NULL_LOCAL_LABEL) {
          //Loop never returns!

          //Loop merge can just become an inline block, no merging needed
          builder->ir->current_block = loop_merge_label;
          builder->ir->set_current_cf(IR::CFInline{
            parent,
              loop_split_label,
          });
        }
        else {
          builder->rescope_variables(parent_variables.size);
          builder->reset_variables();

          builder->ir->set_current_cf(IR::CFInline{
            builder->parent,
              loop_merge_label,
          });

          builder->ir->current_block = loop_merge_label;

          builder->ir->set_current_cf(IR::CFMerge{
            {parent, loop_end},

              loop_split_label,
          });
        }

        builder->variables_state = std::move(branch_variables);
        builder->switch_control_block(escape_label, loop_split_label);
        return;
      }
    case AST_TYPE::IF_ELSE: {
        ASTIfElse* const if_else = (ASTIfElse*)statement;

        const IR::LocalLabel parent = builder->ir->current_block;
        const IR::LocalLabel split_label = builder->ir->new_control_block();

        const IR::LocalLabel if_label = builder->ir->new_control_block();
        const IR::LocalLabel else_label = builder->ir->new_control_block();

        builder->ir->set_current_cf(IR::CFInline{
          builder->parent,
            split_label,
        });

        {
          builder->switch_control_block(split_label, parent);

          Eval::RuntimeValue cond = compile_bytecode(comp, comp_thread,
                                                     builder, NodeEval::new_value(if_else->condition));
          if (comp_thread->is_panic()) {
            return;
          }

          ASSERT(cond.effective_type() == comp_thread->builtin_types->t_bool);

          IR::ValueIndex cond_vi = builder->ir->new_temporary(comp_thread->builtin_types->t_bool, {});


          Eval::assign(builder->ir, Eval::as_direct(cond_vi, comp_thread->builtin_types->t_bool), cond);

          builder->reset_variables();
          builder->ir->set_current_cf(IR::CFSplt{
            parent,
              cond_vi,
              if_label,
              else_label,
          });
        }

        Axle::Array split_variables = copy_arr(builder->variables_state);
        usize scope_size = split_variables.size;

        {
          builder->switch_control_block(if_label, split_label);

          //If branch
          compile_bytecode_of_statement(comp, comp_thread, builder, if_else->if_statement);
          if (comp_thread->is_panic()) {
            return;
          }


          if (builder->ir->current_block != IR::NULL_LOCAL_LABEL) {
            builder->rescope_variables(scope_size);
            builder->reset_variables();
          }
          else {
            builder->variables_state = {};
          }
        }

        IR::LocalLabel if_end_parent = builder->parent;
        IR::LocalLabel if_end = builder->ir->current_block;

        Axle::Array if_variables = std::exchange(builder->variables_state, std::move(split_variables));

        IR::LocalLabel else_end_parent = split_label;
        IR::LocalLabel else_end = else_label;

        if (if_else->else_statement != 0) {
          builder->switch_control_block(else_end, else_end_parent);

          compile_bytecode_of_statement(comp, comp_thread, builder, if_else->else_statement);
          if (comp_thread->is_panic()) {
            return;
          }

          if (builder->ir->current_block != IR::NULL_LOCAL_LABEL) {
            builder->rescope_variables(scope_size);
            builder->reset_variables();
          }
          else {
            builder->variables_state = {};
          }

          else_end_parent = builder->parent;
          else_end = builder->ir->current_block;
        }

        builder->ir->end_control_block();

        if (if_end == IR::NULL_LOCAL_LABEL && else_end == IR::NULL_LOCAL_LABEL) {
          //Unreachable code
          builder->ir->current_block = IR::NULL_LOCAL_LABEL;
          builder->parent = IR::NULL_LOCAL_LABEL;
        }
        else if (if_end == IR::NULL_LOCAL_LABEL) {
          builder->ir->current_block = else_end;
          builder->parent = else_end_parent;
        }
        else if (else_end == IR::NULL_LOCAL_LABEL) {
          builder->ir->current_block = if_end;
          builder->parent = if_end_parent;
        }
        else {
          //Do the merge
          IR::LocalLabel merge_label = builder->ir->new_control_block();
          builder->parent = IR::NULL_LOCAL_LABEL;

          {
            builder->ir->current_block = if_end;
            builder->ir->set_current_cf(IR::CFInline{
              if_end_parent,
                merge_label
            });

            builder->ir->current_block = else_end;
            builder->ir->set_current_cf(IR::CFInline{
              else_end_parent,
                merge_label
            });
          }

          builder->ir->current_block = merge_label;
          IR::LocalLabel final_label = builder->ir->new_control_block();

          builder->ir->set_current_cf(IR::CFMerge{
            {if_end, else_end},
              final_label,
          });

          builder->switch_control_block(final_label, merge_label);
        }
        return;
      }
    case AST_TYPE::DECL: {
        ASTDecl* const decl = (ASTDecl*)statement;

        ASSERT(decl->decl_type == ASTDecl::TYPE::LOCAL);//globals are done elsewhere (maybe move that to here?)
        Local* const local = decl->local_ptr;

        if (local->decl.init_value != nullptr) {
          local->variable_id = builder->new_variable(local->decl.type, {});

          const auto var = builder->import_variable(local->variable_id, {});

          Eval::assign(builder->ir, var, Eval::as_constant(local->decl.init_value, local->decl.type));
        }
        else {
          ASSERT(!decl->compile_time_const);

          Eval::RuntimeValue r = compile_bytecode(comp, comp_thread,
                                                  builder, NodeEval::new_value(decl->expr));
          if (comp_thread->is_panic()) {
            return;
          }

          local->variable_id = builder->new_variable(local->decl.type, {});
          const auto var = builder->import_variable(local->variable_id, {});

          Eval::assign(builder->ir, var, r);
        }

        return;
      }
    default: {
        [[maybe_unused]] auto _ = compile_bytecode(comp, comp_thread, builder, NodeEval::new_value(statement));
        return;
      }
  }
}

void submit_ir(CompilerGlobals* comp, IR::IRStore* ir) {
  if (comp->print_options.finished_ir) {
    IR::print_ir(comp, ir);
  }
  comp->finished_irs.push_back(ir);
}

void IR::eval_ast(CompilerGlobals* comp, CompilerThread* comp_thread, AST_LOCAL root, IR::EvalPromise* eval) {
  TELEMETRY_FUNCTION();

  if (!VC::is_comptime(root->value_category)) {
    comp_thread->report_error(ERROR_CODE::VM_ERROR, root->node_span, "Cannot evaluate a non-comptime expression");
    return;
  }

  IR::IRStore expr_ir = {};
  expr_ir.signature = comp_thread->builtin_types->t_void_call.extract_base<SignatureStructure>();

  Eval::IrBuilder builder = start_builder(Eval::Time::CompileTime, &expr_ir);

  Eval::RuntimeValue ref = compile_bytecode(comp, comp_thread, &builder, NodeEval::new_value(root));
  if (comp_thread->is_panic()) {
    return;
  }

  Eval::end_builder(&builder);

  const Type ref_type = ref.effective_type();
  ASSERT(ref_type == root->node_type);

  if (!eval->type.is_valid()) {
    eval->type = ref_type;
  }
  else {
    ASSERT(eval->type == ref_type);
  }

  const Type& type = ref_type;
  
  if (eval->data == nullptr) {
    eval->data = comp->new_constant(type.size());
  }

  if (ref.rvt == Eval::RVT::Constant) {
    Axle::memcpy_ts(eval->data, type.size(), ref.constant, type.size());
  }
  else {
    IR::V_ARG out = Eval::load_v_arg(&expr_ir, ref);

    {
      VM::Env env = { comp_thread->builtin_types, &comp_thread->errors };
      VM::StackFrame vm = VM::new_stack_frame(&expr_ir);
      VM::exec(&env, &vm);
      if (comp_thread->is_panic()) {
        return;
      }

      auto res = vm.get_value(out);
      ASSERT(res.t == type);

      Axle::memcpy_ts(eval->data, type.size(), res.ptr, type.size());
    }
  }
}

static void compile_structure(CompilerGlobals* comp,
                              CompilerThread* comp_thread,
                              Namespace* names,
                              ASTStructBody* body,
                              const StructCompilation*) {
  TELEMETRY_FUNCTION();

  TC::type_check_ast(comp, comp_thread, names, body, comp_thread->builtin_types->t_type);
}

static void compile_lambda_signature(CompilerGlobals* comp,
                                     CompilerThread* comp_thread,
                                     Namespace* names,
                                     ASTFuncSig* root,
                                     const LambdaSigCompilation*) {
  TELEMETRY_FUNCTION();

  TC::type_check_ast(comp, comp_thread, names, root, {});
}


static void compile_lambda_body(CompilerGlobals* comp,
                                CompilerThread* comp_thread,
                                Namespace* names,
                                ASTLambda* root,
                                const LambdaBodyCompilation* l_comp) {
  TELEMETRY_FUNCTION();

  TC::type_check_ast(comp, comp_thread, names, root, {});
  if (comp_thread->is_panic()) {
    return;
  }

  ASSERT(root->node_type.is_valid());

  {
    IR::IRStore* ir = comp->new_ir(l_comp->func->signature.label, l_comp->func->signature.sig_struct);


    ASSERT(root->sig->ast_type == AST_TYPE::FUNCTION_SIGNATURE);
    ASTFuncSig* func_sig = static_cast<ASTFuncSig*>(root->sig);

    {
      Eval::IrBuilder builder = start_builder(Eval::Time::Runtime, ir, func_sig->parameters);

      {
        AST_ARR arr = ((ASTBlock*)root->body)->block;
        FOR_AST(arr, i) {
          compile_bytecode_of_statement(comp, comp_thread, &builder, i);
          if (comp_thread->is_panic()) {
            return;
          }
        }
      }

      Eval::end_builder(&builder);
    }
    submit_ir(comp, ir);
  }
}


static void compile_export(CompilerGlobals* comp, CompilerThread* comp_thread, Namespace* available_names,
                           ASTExport* root, const ExportCompilation*) {
  TELEMETRY_FUNCTION();

  if (!comp_thread->build_options.is_library) {
    comp_thread->report_error(ERROR_CODE::LINK_ERROR, root->node_span,
                              "Only libraries can dynamically export");
    return;
  }


  TC::type_check_ast(comp, comp_thread, available_names, root, {});
  if (comp_thread->is_panic()) {
    return;
  }

  Axle::Array<Backend::DynExport> exports = {};

  FOR_AST(root->export_list, it) {
    ASSERT(it->ast_type == AST_TYPE::EXPORT_SINGLE);
    const ASTExportSingle* es = static_cast<const ASTExportSingle*>(it);

    IR::EvalPromise eval = {};
    IR::eval_ast(comp, comp_thread, es->value, &eval);
    if (comp_thread->is_panic()) {
      return;
    }

    ASSERT(eval.type.is_valid());

    if (eval.type.struct_type() != STRUCTURE_TYPE::LAMBDA) {
      comp_thread->report_error(ERROR_CODE::LINK_ERROR, it->node_span,
                                "Can only export functions. Found: {}", eval.type.name);
      return;
    }

    ASSERT(eval.data != nullptr);
    IR::GlobalLabel label = *(const IR::GlobalLabel*)eval.data;

    exports.insert(Backend::DynExport{
      es->name,
        label,
    });
  }

  {
    auto program = comp->services.out_program.get();

    program->dyn_exports.concat(std::move(exports));
  }
}

static void compile_import(CompilerGlobals* comp, CompilerThread* comp_thread, Namespace* available_names,
                           ASTImport* root, const ImportCompilation* imp) {
  TELEMETRY_FUNCTION();
  TC::type_check_ast(comp, comp_thread, available_names, root, {});
  if (comp_thread->is_panic()) {
    return;
  }

  IR::EvalPromise p = {};
  IR::eval_ast(comp, comp_thread, root->expr_location, &p);
  if (comp_thread->is_panic()) {
    return;
  }

  Axle::ViewArr<const char> path;

  if (p.type.struct_type() == STRUCTURE_TYPE::FIXED_ARRAY) {
    const ArrayStructure* arr = p.type.unchecked_base<ArrayStructure>();
    if (arr->base != comp->builtin_types->t_ascii) {
      comp_thread->report_error(ERROR_CODE::INTERNAL_ERROR, root->node_span, "Invalid type for import \"{}\"", p.type.name);
      return;
    }

    path = { (const char*)p.data, arr->size };
  }/*
  else if (p.type.struct_type() == STRUCTURE_TYPE::POINTER) {
    const PointerStructure* ptr = p.type.unchecked_base<PointerStructure>();
    if (ptr->base != comp->builtin_types->t_ascii) {
      comp_thread->report_error(ERROR_CODE::INTERNAL_ERROR, root->node_span, "Invalid type for import \"{}\"", p.type.name);
      return;
    }

    path = { (const char*)p.data, arr->size };
  }*/
  else {
    comp_thread->report_error(ERROR_CODE::INTERNAL_ERROR, root->node_span, "Invalid type for import \"{}\"", p.type.name);
    return;
  }

  ASSERT(path.data != nullptr);
  ASSERT(path.size > 0);

  Axle::FileLocation loc;
  bool found = false;

  const Axle::InternString* const paths_to_try[] = {
    imp->src_loc.directory,
    comp->build_options.lib_folder,
    comp->build_options.std_lib_folder,
  };

  for (const Axle::InternString* base : paths_to_try) {
    Axle::AllocFilePath try_path = format_file_path(view_arr(base), path);

    if (Axle::FILES::exists(const_view_arr(try_path.raw))) {
      auto strings = comp->services.strings.get();
      loc = parse_file_location(try_path, strings._ptr);
      found = true;
      break;
    }
  }

  //Didn't find
  if (!found) {
    comp_thread->report_error(ERROR_CODE::FILE_ERROR, root->node_span, "Couldn't find the file specified.\nTried paths: {}",
                              Axle::PrintList<const Axle::InternString*>{paths_to_try, array_size(paths_to_try)});
    return;
  }

  const auto is_correct_file = [&loc](const FileAST* f) {
    return f->file_loc == loc;
  };

  const FileAST* imported_file = comp->parsed_files.find_if(is_correct_file);

  if (imported_file != nullptr) {
    auto names = comp->services.names.get();

    names->add_global_import(&comp_thread->errors, available_names, imported_file->ns, root->node_span);
  }
  else {
    FileImport file_import = {};
    file_import.file_loc = std::move(loc);
    file_import.ns = comp->new_namespace();
    file_import.span = root->node_span;

    {
      auto names = comp->services.names.get();

      //Might error but its probs fine to just leave it as the error will be caught at some point
      names->add_global_import(&comp_thread->errors, available_names, file_import.ns, root->node_span);
    }

    comp->services.file_loader.get()->unparsed_files.insert(std::move(file_import));
  }
}

void compile_global(CompilerGlobals* comp, CompilerThread* comp_thread,
                    Namespace* available_names,
                    ASTDecl* decl, GlobalCompilation* global_comp) {
  TELEMETRY_FUNCTION();

  Global* global = global_comp->global;

  TC::type_check_ast(comp, comp_thread, available_names, decl, {});
  if (comp_thread->is_panic()) {
    return;
  }

  ASSERT(global->decl.type.is_valid());

  if (VC::is_comptime(global->decl.value_category)) {
    ASSERT(global->decl.init_value != nullptr);

    if (global->decl.name == comp->build_options.entry_point) {
      if (global->decl.type.struct_type() != STRUCTURE_TYPE::LAMBDA) {
        comp_thread->report_error(ERROR_CODE::LINK_ERROR, global->decl.span, "Entry point must be a function");
        return;
      }

      if (comp->entry_point_label != IR::NULL_GLOBAL_LABEL) {
        comp_thread->report_error(ERROR_CODE::LINK_ERROR, global->decl.span, "Found a second entry point");
        return;
      }

      comp->entry_point_label = *(IR::GlobalLabel*)(global->decl.init_value);
    }
  }
  else if (global->decl.init_value == nullptr) {
    const SignatureStructure* sig = (const SignatureStructure*)comp->builtin_types->t_void_call.structure;
    IR::GlobalLabel label = comp->next_function_label(sig, global->decl.span);

    IR::IRStore* ir = comp->new_ir(label, sig);

    {
      Eval::IrBuilder builder = Eval::start_builder(Eval::Time::Runtime, ir);

      global->is_runtime_available = true;
      global->dynamic_init_index = new_dynamic_init_object(comp,
                                                           global->decl.type.structure->size,
                                                           global->decl.type.structure->alignment,
                                                           ir);

      Eval::RuntimeValue glob_ref = load_data_memory(comp, &builder, global, {});

      Eval::RuntimeValue init_expr = compile_bytecode(comp, comp_thread, &builder, NodeEval::new_value(decl->expr));
      if (comp_thread->is_panic()) {
        return;
      }

      Eval::assign(ir, glob_ref, init_expr);

      Eval::end_builder(&builder);
    }

    submit_ir(comp, ir);
  }

  {
    auto names = comp->services.names.get();

    names->add_global_name(&comp_thread->errors, available_names, global->decl.name, global);
  }
}

void compile_current_unparsed_files(CompilerGlobals* const comp,
                                    CompilerThread* const comp_thread,
                                    FileLoader* file_loader) {
  TELEMETRY_FUNCTION();

  while (file_loader->unparsed_files.size > 0) {
    //still have files to parse

    //parse the last file
    const FileImport* file_import = file_loader->unparsed_files.back();
    const Axle::InternString* full_path = file_import->file_loc.full_name;

    if (comp->print_options.file_loads) {
      IO::format("Loading file \"{}\" ...\n", full_path);
    }

    //Just a sanity check - should alread have been set
    if (file_import->file_loc.extension == nullptr
        || file_import->file_loc.extension == comp->important_names.axl) {
      //Load a source file

      Axle::OwnedArr<const u8> text_source = Axle::FILES::read_full_file(const_view_arr(full_path));

      if (text_source.data == nullptr) {
        comp_thread->report_error(ERROR_CODE::FILE_ERROR, file_loader->unparsed_files.back()->span,
                                  "File '{}' could not be opened, perhaps it does not exist",
                                  full_path);
        return;
      }


      Parser parser = {};
      parser.file_path = file_import->file_loc;

      //Loaded file can pop of the file stack thing
      //It will shortly be loaded into the parsed files
      file_loader->unparsed_files.pop();

      //Reset the parser for this file
      reset_parser(comp, comp_thread, &parser, full_path, cast_arr<const char>(const_view_arr(text_source)));

      // ^ Can error (in the lexing)
      if (comp_thread->is_panic()) {
        return;
      }

      //Parse
      comp->parsed_files.insert_uninit(1);
      FileAST* ast_file = comp->parsed_files.back();
      ast_file->file_loc = file_import->file_loc;

      ast_file->ns = file_import->ns;
      ast_file->ns->imported.insert(comp->builtin_namespace);

      {
        TELEMETRY_SCOPE("Parsing");

        parser.current_namespace = file_import->ns;

        parse_file(comp, comp_thread, &parser, ast_file);
        if (comp_thread->is_panic()) {
          return;
        }
      }

      if (comp->print_options.ast) {
        Axle::IO_Single::lock();
        DEFER() {
          Axle::IO_Single::unlock();
        };
        Axle::IO_Single::print("\n=== Print Parsed AST ===\n\n");
        print_full_ast(ast_file);
        Axle::IO_Single::print("\n========================\n\n");
      }

      if (comp_thread->is_panic()) {
        return;
      }
    }
    else {
      comp_thread->report_error(ERROR_CODE::FILE_ERROR, file_import->span,
                                "'{}' is not a loadable file extension",
                                file_import->file_loc.extension);
      return;
    }
  }
}

void add_comp_unit_for_import(CompilerGlobals* const comp, Namespace* ns, const Axle::FileLocation& src_loc, ASTImport* imp) noexcept {

  CompilationUnit* imp_unit;
  {
    auto compilation = comp->services.compilation.get();
    ImportCompilation* extra = compilation->import_compilation.allocate();
    extra->src_loc = src_loc;

    imp_unit = new_compilation_unit(compilation._ptr,
                                    COMPILATION_EMIT_TYPE::IMPORT, &comp->pipelines.comp_import,
                                    ns, imp,
                                    (void*)extra, comp->print_options.comp_units);
  }

  ASSERT(imp_unit != nullptr);
  comp->pipelines.depend_check.push_back(imp_unit);
}

void add_comp_unit_for_export(CompilerGlobals* const comp, Namespace* ns, ASTExport* e) noexcept {

  CompilationUnit* exp_unit;
  {
    auto compilation = comp->services.compilation.get();
    ExportCompilation* extra = compilation->export_compilation.allocate();

    exp_unit = new_compilation_unit(compilation._ptr,
                                    COMPILATION_EMIT_TYPE::EXPORT, &comp->pipelines.comp_export,
                                    ns, e,
                                    extra, comp->print_options.comp_units);
  }

  ASSERT(exp_unit != nullptr);
  comp->pipelines.depend_check.push_back(exp_unit);
}

void add_comp_unit_for_global(CompilerGlobals* const comp, Namespace* ns, ASTDecl* global) noexcept {
  ASSERT(global->decl_type == ASTDecl::TYPE::GLOBAL);

  Global* glob = comp->new_global();

  CompilationUnit* glob_unit;
  {
    auto compilation = comp->services.compilation.get();
    GlobalCompilation* extra = compilation->global_compilation.allocate();
    extra->global = glob;
    glob_unit = new_compilation_unit(compilation._ptr,
                                     COMPILATION_EMIT_TYPE::GLOBAL, &comp->pipelines.comp_global,
                                     ns, global,
                                     (void*)extra, comp->print_options.comp_units);
  }

  glob->decl.name = global->name;
  glob->decl.span = global->node_span;

  global->global_ptr = glob;

  ASSERT(glob_unit != nullptr);
  comp->pipelines.depend_check.push_back(glob_unit);
}

void add_comp_unit_for_lambda(CompilerGlobals* const comp, Namespace* ns, ASTLambda* lambda) noexcept {
  TELEMETRY_FUNCTION();

  //Setup the function object
  IR::Function* const func = comp->new_function();
  lambda->function = func;

  func->declaration = lambda;

  ASTFuncSig* func_sig = (ASTFuncSig*)(lambda->sig);
  func_sig->sig = &func->signature;
  func_sig->convention = comp->build_options.default_calling_convention;

  //Set the compilation units
  CompilationUnit* sig_unit;
  {
    auto compilation = comp->services.compilation.get();
    LambdaSigCompilation* sig_extra = compilation->lambda_sig_compilation.allocate();
    sig_extra->func = func;
    sig_extra->lambda = lambda;


    sig_unit = new_compilation_unit(compilation._ptr,
                                    COMPILATION_EMIT_TYPE::LAMBDA_SIG, &comp->pipelines.comp_signature,
                                    ns, func_sig,
                                    sig_extra, comp->print_options.comp_units);

    func->sig_unit_id = sig_unit->id;
  }

  comp->pipelines.depend_check.push_back(sig_unit);
}

void add_comp_unit_for_struct(CompilerGlobals* const comp, Namespace* ns, ASTStructBody* struct_body) noexcept {
  CompilationUnit* unit;
  {
    auto compilation = comp->services.compilation.get();
    StructCompilation* struct_extra = compilation->struct_compilation.allocate();

    unit = new_compilation_unit(compilation._ptr,
                                COMPILATION_EMIT_TYPE::STRUCTURE, &comp->pipelines.comp_structure,
                                ns, struct_body,
                                struct_extra, comp->print_options.comp_units);

    struct_body->unit_id = unit->id;
  }

  ASSERT(unit != nullptr);
  comp->pipelines.depend_check.push_back(unit);
}

void DependencyManager::try_restart(CompilationUnit* unit, bool print) {
  ASSERT(unit != nullptr);

  if (!unit->waiting()) {
    if (print) {
      IO::format("Remove Dependency from unit {} -> starting again\n", unit->id);
    }

    ASSERT(depend_check_pipe != nullptr);

    depend_check_pipe->push_back(unit);//has to re-run through depend check
    in_flight_units += 1;
  }
}

void DependencyManager::add_dependency_to(CompilationUnit* now_waiting, CompilationUnit* waiting_on) {
  ASSERT(now_waiting != nullptr);
  ASSERT(waiting_on != nullptr);

  now_waiting->unit_wait_on_count += 1;
  waiting_on->depend_list_size += 1;

  DependencyListSingle* old_top = waiting_on->dependency_list;
  waiting_on->dependency_list = dependency_list_entry.allocate();

  waiting_on->dependency_list->waiting = now_waiting;
  waiting_on->dependency_list->next = old_top;
}

void DependencyManager::close_dependency(CompilationUnit* ptr, bool print) {
  ASSERT(ptr != nullptr);
  usize i = 0;
  DependencyListSingle* dep_single = ptr->dependency_list;

  while (dep_single != nullptr) {
    CompilationUnit* waiting = dep_single->waiting;
    DependencyListSingle* next = dep_single->next;
    i += 1;
    dependency_list_entry.free(dep_single);
    dep_single = next;

    waiting->unit_wait_on_count -= 1;
    try_restart(waiting, print);
  }

  ASSERT(i == ptr->depend_list_size);
  in_flight_units -= 1;
}

//Might be that dependencies were already dispatched
//Return true if depended
bool try_dispatch_dependencies(CompilerGlobals* comp, CompilerThread* comp_thread, CompilationUnit* unit) {
  TELEMETRY_FUNCTION();

  ASSERT(unit != nullptr);

  auto compilation = comp->services.compilation.get();

  bool depended = false;
  FOR(comp_thread->new_depends, id) {
    CompilationUnit* u = compilation->store.get_unit_if_exists(*id);
    if (u != nullptr) {
      if (comp_thread->print_options.comp_units) {
        IO::format("Comp unit {} waiting on {}\n",
                     unit->id, u->id);
      }

      depended = true;
      compilation->dependencies.add_dependency_to(unit, u);
    }
  }
  comp_thread->new_depends.clear();

  if (comp_thread->local_unfound_names.names.size > 0) {
    depended = true;

    FOR_MUT(comp_thread->local_unfound_names.names, it) {
      it->dependency = unit;
      unit->unfound_wait_on_count += 1;
    }

    if (comp_thread->print_options.comp_units) {
      const Axle::ViewArr<UnfoundNameHolder> names = view_arr(comp_thread->local_unfound_names.names);
      IO::format("Comp unit {} waiting on {}\n", unit->id, Axle::PrintListCF{names.data, names.size, 
                 [](Format::Formatter auto& res, const UnfoundNameHolder& nh) {
        res.load_char('"');
        Format::FormatArg<const Axle::InternString*>::load_string(res, nh.name.ident);
        res.load_char('"');
      }});
    }
    compilation->unfound_names.names.concat(std::move(comp_thread->local_unfound_names.names));
    comp_thread->local_unfound_names.names.clear();
  }

  if (depended) {
    compilation->dependencies.in_flight_units -= 1;
    if (comp_thread->print_options.comp_units) {
      IO::format("Comp unit {} now waiting   | Active = {}, In flight = {}\n",
                   unit->id, compilation->store.active_units.size, compilation->dependencies.in_flight_units);
    }
  }

  return depended;
}

void close_compilation_unit(CompilerThread* comp_thread, Compilation* compilation, CompilationUnit* unit) {
  TELEMETRY_FUNCTION();

  UnitID id = unit->id;

  ASSERT(compilation->dependencies.in_flight_units != 0);
  compilation->dependencies.close_dependency(unit, comp_thread->print_options.comp_units);

  compilation->store.free_unit(unit);

  if (comp_thread->print_options.comp_units) {
    IO::format("Close Comp unit {}         | Active = {}, In flight = {}\n",
                 id, compilation->store.active_units.size, compilation->dependencies.in_flight_units);
  }
}

void run_compiler_pipes(CompilerGlobals* const comp, CompilerThread* const comp_thread) {

  {
    bool acquired = comp->services.file_loader._mutex.acquire_if_free();

    if (acquired) {
      FileLoader* file_loader = comp->services.file_loader._ptr;

      DEFER(comp) {
        comp->services.file_loader._mutex.release();
      };

      if (file_loader->unparsed_files.size > 0) {
        TELEMETRY_SCOPE("Parse Files");
        thead_doing_work(comp, comp_thread);

        if (comp->print_options.work) {
          IO::format("Work | Parsing {} Files \n", file_loader->unparsed_files.size);
        }

        compile_current_unparsed_files(comp, comp_thread, file_loader);
        if (comp_thread->is_panic()) {
          return;
        }

        ASSERT(!comp_thread->is_depends());
        return;
      }
    }
  }

  {
    bool acquired = comp->services.out_program._mutex.acquire_if_free();

    if (acquired) {
      DEFER(comp) {
        comp->services.out_program._mutex.release();
      };
      Backend::ProgramData* p = comp->services.out_program._ptr;

      const IR::IRStore* ir = nullptr;
      if (comp->finished_irs.try_pop_front(&ir)) {
        TELEMETRY_SCOPE("Emit IR");
        thead_doing_work(comp, comp_thread);

        if (comp->print_options.work) {
          IO::format("Work | Output Finished IR\n");
        }

        comp_thread->platform_interface.emit_function(comp,
                                                      comp_thread, ir,
                                                      comp->build_options.default_calling_convention,
                                                      p);
        if (comp_thread->is_panic()) {
          return;
        }

        ASSERT(!comp_thread->is_depends());
        return;
      }
    }
  }

  CompilationUnit* unit = nullptr;

  if (comp->pipelines.comp_import.try_pop_front(&unit)) {
    TELEMETRY_SCOPE("Import");
    thead_doing_work(comp, comp_thread);
    if (comp->print_options.work) {
      IO::format("Work | Import {}\n", unit->id);
    }

    ASSERT(!comp_thread->is_depends() && !comp_thread->is_panic());

    ASSERT(!unit->waiting());

    ASSERT(unit->detail != nullptr);
    ImportCompilation* imp = (ImportCompilation*)unit->detail;

    ASSERT(unit->ast != nullptr);
    ASTImport* root = downcast_ast<ASTImport>(unit->ast);

    compile_import(comp, comp_thread, unit->available_names, root, imp);
    if (comp_thread->is_panic()) {
      return;
    }

    //Finished
    auto compilation = comp->services.compilation.get();

    compilation->import_compilation.free(imp);
    close_compilation_unit(comp_thread, compilation._ptr, unit);
    return;
  }

  if (comp->pipelines.comp_body.try_pop_front(&unit)) {
    TELEMETRY_SCOPE("Compile Lambda Body");
    thead_doing_work(comp, comp_thread);

    if (comp->print_options.work) {
      IO::format("Work | Work {}\n", unit->id);
    }

    ASSERT(!comp_thread->is_depends() && !comp_thread->is_panic());
    ASSERT(!unit->waiting());

    LambdaBodyCompilation* lbc = (LambdaBodyCompilation*)unit->detail;

    ASSERT(unit->ast != nullptr);
    ASTLambda* lambda = downcast_ast<ASTLambda>(unit->ast);

    compile_lambda_body(comp, comp_thread, unit->available_names, lambda, lbc);
    if (comp_thread->is_panic()) {
      return;
    }

    //Finished
    auto compilation = comp->services.compilation.get();

    compilation->lambda_body_compilation.free(lbc);
    close_compilation_unit(comp_thread, compilation._ptr, unit);
    return;
  }

  if (comp->pipelines.comp_global.try_pop_front(&unit)) {
    TELEMETRY_SCOPE("Emit Global");
    thead_doing_work(comp, comp_thread);
    if (comp->print_options.work) {
      IO::format("Work | Global {}\n", unit->id);
    }

    ASSERT(!comp_thread->is_depends() && !comp_thread->is_panic());

    ASSERT(!unit->waiting());

    ASSERT(unit->ast != nullptr);

    ASTDecl* decl = downcast_ast<ASTDecl>(unit->ast);
    GlobalCompilation* global_extra = (GlobalCompilation*)unit->detail;

    compile_global(comp, comp_thread, unit->available_names, decl, global_extra);
    if (comp_thread->is_panic()) {
      return;
    }

    //Finished
    auto compilation = comp->services.compilation.get();

    compilation->global_compilation.free(global_extra);
    close_compilation_unit(comp_thread, compilation._ptr, unit);
    return;
  }

  if (comp->pipelines.comp_structure.try_pop_front(&unit)) {
    TELEMETRY_SCOPE("Compile Structure");
    thead_doing_work(comp, comp_thread);
    if (comp->print_options.work) {
      IO::format("Work | Structure {}\n", unit->id);
    }

    ASSERT(!comp_thread->is_depends() && !comp_thread->is_panic());

    ASSERT(!unit->waiting());

    ASSERT(unit->ast != nullptr);

    ASTStructBody* body = downcast_ast<ASTStructBody>(unit->ast);
    const StructCompilation* struct_extra = (StructCompilation*)unit->detail;

    compile_structure(comp, comp_thread, unit->available_names, body, struct_extra);
    if (comp_thread->is_panic()) {
      return;
    }

    //Finished
    auto compilation = comp->services.compilation.get();

    compilation->struct_compilation.free(struct_extra);
    close_compilation_unit(comp_thread, compilation._ptr, unit);
    return;
  }

  if (comp->pipelines.comp_signature.try_pop_front(&unit)) {
    TELEMETRY_SCOPE("Compile Lambda Signature");
    thead_doing_work(comp, comp_thread);

    if (comp->print_options.work) {
      IO::format("Work | Signature {}\n", unit->id);
    }

    ASSERT(!comp_thread->is_depends() && !comp_thread->is_panic());

    LambdaSigCompilation* lsc = (LambdaSigCompilation*)unit->detail;
    ASSERT(unit->ast != nullptr);
    ASTFuncSig* sig = downcast_ast<ASTFuncSig>(unit->ast);

    compile_lambda_signature(comp, comp_thread, unit->available_names, sig, lsc);
    if (comp_thread->is_panic()) {
      return;
    }

    //Finished
    auto compilation = comp->services.compilation.get();

    CompilationUnit* body_unit;
    {
      ASTLambda* lambda = lsc->lambda;
      IR::Function* func = lsc->func;
      Namespace* available_names = unit->available_names;

      compilation->lambda_sig_compilation.free(lsc);
      close_compilation_unit(comp_thread, compilation._ptr, unit);

      //Create the unit for the body
      LambdaBodyCompilation* lbc = compilation->lambda_body_compilation.allocate();
      lbc->func = func;

      body_unit = new_compilation_unit(compilation._ptr, COMPILATION_EMIT_TYPE::LAMBDA_BODY, &comp->pipelines.comp_body,
                                       available_names, lambda, lbc, comp->print_options.comp_units);
    }

    comp->pipelines.depend_check.push_back(body_unit);

    return;
  }

  if (comp->pipelines.comp_export.try_pop_front(&unit)) {
    TELEMETRY_SCOPE("Compile Export");
    thead_doing_work(comp, comp_thread);

    if (comp->print_options.work) {
      IO::format("Work | Export {}\n", unit->id);
    }

    ASSERT(!comp_thread->is_depends() && !comp_thread->is_panic());
    ASSERT(!unit->waiting());

    ExportCompilation* ec = (ExportCompilation*)unit->detail;

    ASSERT(unit->ast != nullptr);
    ASTExport* export_ast = downcast_ast<ASTExport>(unit->ast);

    compile_export(comp, comp_thread, unit->available_names, export_ast, ec);
    if (comp_thread->is_panic()) {
      return;
    }

    //Finished
    auto compilation = comp->services.compilation.get();

    compilation->export_compilation.free(ec);
    close_compilation_unit(comp_thread, compilation._ptr, unit);
    return;
  }

  if (comp->pipelines.depend_check.try_pop_front(&unit)) {
    TELEMETRY_SCOPE("Depend check");
    thead_doing_work(comp, comp_thread);

    if (comp->print_options.work) {
      IO::format("Work | Depend Check {}\n", unit->id);
    }

    ASSERT(!comp_thread->is_depends() && !comp_thread->is_panic());

    ASSERT(!unit->waiting());

    DependencyChecker st = {};
    st.available_names = unit->available_names;

    ASSERT(unit->ast != nullptr);

    dependency_check_ast_node(comp, comp_thread, &st, unit->ast);
    if (comp_thread->is_panic()) {
      return;
    }

    if (comp_thread->is_depends()) {
      [[maybe_unused]] bool depended = try_dispatch_dependencies(comp, comp_thread, unit);
    }
    else {
      unit->main_pipe->push_back(unit);
    }
    return;
  }

  ASSERT(unit == nullptr);//shouldn't have been modified in this time

  {
    auto compilation = comp->services.compilation.get();

    //Wait for there to be no compiling to check unfound deps - best chance they exist
    if (compilation->unfound_names.names.size > 0) {
      TELEMETRY_SCOPE("Check Unfound Names");

      usize num_unfound = compilation->unfound_names.names.size;

      auto names = comp->services.names.get();

      const auto found_dep_l = [comp_thread, names = names._ptr, dep_ptr = &compilation->dependencies](const UnfoundNameHolder& dep) -> bool {
        const GlobalName* gn = names->find_global_name(dep.name.ns, dep.name.ident);

        if (gn != nullptr) {
          dep.dependency->unfound_wait_on_count -= 1;
          dep_ptr->try_restart(dep.dependency, comp_thread->print_options.comp_units);
          return true;
        }
        else {
          return false;
        }
      };

      //Remove units if dependency has been found
      compilation->unfound_names.names.remove_if(found_dep_l);

      if (num_unfound != compilation->unfound_names.names.size) {
        thead_doing_work(comp, comp_thread);
      }
    }
  }


  //IO::format("---- DEBUG: Did nothing in pass\n");

  if (comp_thread->doing_work) {
    comp_thread->doing_work = false;
    comp->work_counter -= 1;
    return;
  }
}

void compiler_loop(CompilerGlobals* const comp, CompilerThread* const comp_thread) {
  TELEMETRY_FUNCTION();

  {//force reset
    comp_thread->doing_work = false;
    thead_doing_work(comp, comp_thread);
  }

  while (!comp->is_global_panic() && (comp_thread->doing_work || comp->work_counter > 0)) {
    try {
      run_compiler_pipes(comp, comp_thread);
    }
    catch (const std::exception& e) {
      const char* message = e.what();
      const Axle::ViewArr<const char> message_view = { message, Axle::strlen_ts(message) };
      comp_thread->report_error(ERROR_CODE::ASSERT_ERROR, Span{}, "Assertion Failed with message: {}", message_view);
    }

    if (comp_thread->is_panic()) {
      comp->global_panic.set();
    }
  }

  if (comp_thread->is_panic()) {
    ASSERT(comp->is_global_panic());

    comp->global_errors_mutex.acquire();
    comp->global_errors.concat(std::move(comp_thread->errors.error_messages));
    comp->global_errors_mutex.release();
  }
}

struct ThreadData {
  CompilerGlobals* comp;
  CompilerThread* comp_thread;
};

void compiler_loop_thread_proc(const Axle::ThreadHandle*, void* data) {
  ASSERT(data != nullptr);
  ThreadData* t_data = (ThreadData*)data;
  CompilerGlobals* comp = t_data->comp;
  CompilerThread* comp_thread = t_data->comp_thread;
  ASSERT(comp != nullptr);

  compiler_loop(comp, comp_thread);
}

void compiler_loop_threaded(CompilerGlobals* const comp, CompilerThread* const comp_thread) {
  const usize extra_threads = (usize)comp->active_threads - 1;
  ASSERT(comp_thread->thread_id == 0);

  copy_compiler_constants(comp, comp_thread);//copy to this thread every time

  if (extra_threads > 0) {
#if defined(AXLE_TRACING) && defined(TRACING_ENABLE)
    Tracing::Event tracing_start_threads = Tracing::start_event("Start Threads");
#endif
    Axle::OwnedArr<ThreadData> thread_datas = Axle::new_arr<ThreadData>(extra_threads);
    Axle::OwnedArr<CompilerThread> comp_threads = Axle::new_arr<CompilerThread>(extra_threads);
    Axle::OwnedArr<const Axle::ThreadHandle*> handles = Axle::new_arr<const Axle::ThreadHandle*>(extra_threads);

    for (usize i = 0; i < extra_threads; i++) {
      thread_datas[i].comp = comp;
      thread_datas[i].comp_thread = comp_threads.data + i;
      comp_threads[i].thread_id = (u32)(i + 1);
      copy_compiler_constants(comp, comp_threads.data + i);
    }

    //Start the threads
    for (usize i = 0; i < extra_threads; i++) {
      handles[i] = start_thread(compiler_loop_thread_proc, thread_datas.data + i);
    }

#if defined(AXLE_TRACING) && defined(TRACING_ENABLE)
    Tracing::end_event(tracing_start_threads);
#endif

    compiler_loop(comp, comp_thread);

    {
      TELEMETRY_SCOPE("Close Threads");
      for (usize i = 0; i < extra_threads; i++) {
        wait_for_thread_end(handles[i]);
      }

    }
  }
  else {
    compiler_loop(comp, comp_thread);
  }
}

static void free_remaining_compilation_units(Compilation* compilation) {
  for (CompilationUnit* unit : compilation->store.active_units) {
    auto dep = unit->dependency_list;
    while (dep != nullptr) {
      auto next = dep->next;
      compilation->dependencies.dependency_list_entry.free(dep);
      dep = next;
    }

    switch (unit->emit) {
      case COMPILATION_EMIT_TYPE::STRUCTURE: {
          compilation->struct_compilation.free((const StructCompilation*)unit->detail);
          break;
        }
      case COMPILATION_EMIT_TYPE::LAMBDA_BODY: {
          compilation->lambda_body_compilation.free((const LambdaBodyCompilation*)unit->detail);
          break;
        }
      case COMPILATION_EMIT_TYPE::LAMBDA_SIG: {
          compilation->lambda_sig_compilation.free((const LambdaSigCompilation*)unit->detail);
          break;
        }
      case COMPILATION_EMIT_TYPE::GLOBAL: {
          compilation->global_compilation.free((const GlobalCompilation*)unit->detail);
          break;
        }
      case COMPILATION_EMIT_TYPE::IMPORT: {
          compilation->import_compilation.free((const ImportCompilation*)unit->detail);
          break;
        }
      case COMPILATION_EMIT_TYPE::EXPORT: {
          compilation->export_compilation.free((const ExportCompilation*)unit->detail);
          break;
        }
    }

    compilation->store.compilation_units.free(unit);
  }
}

void compile_all(CompilerGlobals* const comp, CompilerThread* const comp_thread) {
  TELEMETRY_FUNCTION();

  ASSERT(comp->active_threads >= 1);

  compiler_loop_threaded(comp, comp_thread);
  DEFER(comp) {
    free_remaining_compilation_units(comp->services.compilation.get()._ptr);
  };

  if (comp->is_global_panic()) {
    return;
  }


  {
    Axle::AtomicLock<Compilation> compilation;
    Axle::AtomicLock<FileLoader> files;
    comp->services.get_multiple(&files, &compilation);

    if (compilation->unfound_names.names.size > 0) {
      auto& names = compilation->unfound_names.names;

      //All names are still unfound
      auto i = names.mut_begin();
      auto end = names.mut_end();

      comp->global_errors.reserve_extra(names.size);

      for (; i < end; ++i) {
        comp->global_errors.insert(std::move(i->as_error));
      }

      names.clear();

      comp->global_panic.set();
      return;
    }

    if (comp->work_counter == 0
        && (compilation->dependencies.in_flight_units > 0
            || compilation->store.active_units.size > 0
            || comp->finished_irs.size > 0
            || files->unparsed_files.size > 0)) {
      comp->work_counter += 1;

      Format::ArrayFormatter error = {};
      Format::format_to(error, "Work still exists but is not accessable according to these counters:\n");
      Format::format_to(error, "- In flight units: {}\n", compilation->dependencies.in_flight_units);
      Format::format_to(error, "- Active units: {}\n", compilation->store.active_units.size);
      Format::format_to(error, "- Finished IRs: {}\n", comp->finished_irs.size);
      Format::format_to(error, "- Unparsed files: {}\n", files->unparsed_files.size);

      error.load_string_lit("\n");

      auto i = compilation->store.active_units.begin();
      const auto end = compilation->store.active_units.end();

      if (i < end) {
        Format::format_to(error, "The following compilation units are inaccessable: \n");
        for (; i < end; ++i) {
          CompilationUnit* unit = *i;
          ASSERT(unit != nullptr);
          ASSERT(unit->main_pipe != nullptr);
          Axle::ViewArr<const char> debug_name = unit->main_pipe->_debug_name;
          if (debug_name.data == nullptr) {
            debug_name = Axle::lit_view_arr("Type unsupported in this mode");
          }

          Format::format_to(error, "- Id: {} | Type: {}\n"
                            "  Waiting on Units: {} | Waiting on Names: {}\n",
                            unit->id, debug_name, unit->unit_wait_on_count, unit->unfound_wait_on_count);
        }
      }

      Format::format_to(error, "Pipeline states:\n");

      comp->pipelines.depend_check.mutex.acquire();
      Format::format_to(error, "- Depend Check: {}\n", comp->pipelines.depend_check.size);
      comp->pipelines.depend_check.mutex.release();

      comp->pipelines.comp_structure.mutex.acquire();
      Format::format_to(error, "- Compile Structure: {}\n", comp->pipelines.comp_structure.size);
      comp->pipelines.comp_structure.mutex.release();

      comp->pipelines.comp_body.mutex.acquire();
      Format::format_to(error, "- Compile Lambda Body: {}\n", comp->pipelines.comp_body.size);
      comp->pipelines.comp_body.mutex.release();

      comp->pipelines.comp_signature.mutex.acquire();
      Format::format_to(error, "- Compile Lambda Signature: {}\n", comp->pipelines.comp_signature.size);
      comp->pipelines.comp_signature.mutex.release();

      comp->pipelines.comp_global.mutex.acquire();
      Format::format_to(error, "- Compile Global: {}\n", comp->pipelines.comp_global.size);
      comp->pipelines.comp_global.mutex.release();

      comp->pipelines.comp_import.mutex.acquire();
      Format::format_to(error, "- Compile Import: {}\n", comp->pipelines.comp_import.size);
      comp->pipelines.comp_import.mutex.release();

      comp->pipelines.comp_export.mutex.acquire();
      Format::format_to(error, "- Compile Export: {}\n", comp->pipelines.comp_export.size);
      comp->pipelines.comp_export.mutex.release();

      comp_thread->report_error(ERROR_CODE::INTERNAL_ERROR, Span{}, error.take());
      comp->global_panic.set();
      comp->global_errors_mutex.acquire();
      comp->global_errors.concat(std::move(comp_thread->errors.error_messages));
      comp->global_errors_mutex.release();
      return;
    }
  }

  ASSERT(comp->services.compilation.get()->dependencies.in_flight_units == 0);
  ASSERT(comp->services.compilation.get()->store.active_units.size == 0);

  {
    TELEMETRY_SCOPE("Load Imports");

    auto p = comp->services.out_program.get();

    FOR(comp->dyn_lib_imports, it) {
      comp->platform_interface.emit_dyn_library_function(comp_thread, it, comp->build_options.default_calling_convention,
                                                         p._ptr);

      if (comp_thread->is_panic()) {
        return;
      }
    }
  }

  if (!comp->build_options.is_library) {
    TELEMETRY_SCOPE("Create Entry Point");
    
    if (comp->entry_point_label == IR::NULL_GLOBAL_LABEL) {
      comp_thread->report_error(ERROR_CODE::LINK_ERROR, Span{}, "Did not find entry point (expected name = \"{}\")",
                                comp->build_options.entry_point);
      return;
    }

    comp->platform_interface.emit_start(comp, comp->entry_point_label, comp->services.out_program._ptr);
  }
}

void create_named_type(CompilerGlobals* comp, CompilerThread* comp_thread,
                       NameManager* names, const Span& span, Namespace* ns,
                       const Type& type) {
  Global* g = comp->new_global();

  ASSERT(type.is_valid());

  //Make sure that t_type is already created before this since we need it
  ASSERT(comp->builtin_types->t_type.is_valid());

  g->decl.value_category = VALUE_CATEGORY::VARIABLE_CONSTANT;
  g->decl.name = type.name;
  g->decl.type = comp->builtin_types->t_type;
  g->decl.span = span;
  g->decl.init_value = (const u8*)comp->new_constant<Type>();

  Axle::memcpy_ts((Type*)g->decl.init_value, 1, &type, 1);

  names->add_global_name(&comp_thread->errors, ns, type.name, g);
}

void create_named_enum_value(CompilerGlobals* comp, CompilerThread* comp_thread,
                             NameManager* names, const Span& span, Namespace* ns,
                             const EnumValue* v) {
  Global* g = comp->new_global();

  ASSERT(v->type.is_valid());

  g->decl.value_category = VALUE_CATEGORY::VARIABLE_CONSTANT;
  g->decl.name = v->name;
  g->decl.type = v->type;
  g->decl.span = span;
  g->decl.init_value = (const u8*)comp->new_constant<const EnumValue*>();

  Axle::memcpy_ts((const EnumValue**)g->decl.init_value, 1, &v, 1);

  names->add_global_name(&comp_thread->errors, ns, v->name, g);
}

void init_compiler(const APIOptions& options, CompilerGlobals* comp, CompilerThread* comp_thread) {
  TELEMETRY_FUNCTION();

  comp_thread->thread_id = 0;//first thread is thread 0

  //Init the types
  auto file_loader = comp->services.file_loader.get();
  auto names = comp->services.names.get();
  auto structures = comp->services.structures.get();
  auto strings = comp->services.strings.get();
  auto* builtin_types = comp->builtin_types;


  //Setup the built in namespace
  Namespace* builtin_namespace = comp->new_namespace();
  comp->builtin_namespace = builtin_namespace;
  comp->platform_interface = *options.platform_interface;

  const auto register_builtin_type = [names = names._ptr, comp_thread, comp, builtin_namespace](const Type& t) {
    create_named_type(comp, comp_thread, names, Span{}, builtin_namespace, t);
    ASSERT(!comp_thread->is_panic());
  };

  const auto register_builtin_enum_value = [names = names._ptr, comp_thread, comp, builtin_namespace](const EnumValue* v) {
    create_named_enum_value(comp, comp_thread, names, Span{}, builtin_namespace, v);
    ASSERT(!comp_thread->is_panic());
  };

  structures->pointer_size = comp->platform_interface.ptr_size;

  *builtin_types = STRUCTS::create_builtins(structures._ptr, strings._ptr);

  register_builtin_type(builtin_types->t_type);
  register_builtin_type(builtin_types->t_void);

  register_builtin_type(builtin_types->t_u8);
  register_builtin_type(builtin_types->t_i8);
  register_builtin_type(builtin_types->t_u16);
  register_builtin_type(builtin_types->t_i16);
  register_builtin_type(builtin_types->t_u32);
  register_builtin_type(builtin_types->t_i32);
  register_builtin_type(builtin_types->t_u64);
  register_builtin_type(builtin_types->t_i64);
  
  register_builtin_type(builtin_types->t_void_ptr);
  register_builtin_type(builtin_types->t_void_call);
  register_builtin_type(builtin_types->t_ascii);
  
  register_builtin_type(builtin_types->t_bool);
  register_builtin_enum_value(builtin_types->e_true);
  register_builtin_enum_value(builtin_types->e_false);

  {
    //Nullptr
    Global* g = comp->new_global();

    g->decl.name = strings->intern("nullptr", 7);
    g->decl.span = Span{};
    g->decl.type = builtin_types->t_void_ptr;
    g->decl.value_category = VALUE_CATEGORY::VARIABLE_CONSTANT;
    g->decl.init_value = (const u8*)comp->new_constant<const u8*>();
    *(const u8**)g->decl.init_value = 0;//This is disgusting

    names->add_global_name(&comp_thread->errors, builtin_namespace, g->decl.name, g);
  }

  //Intrinsics
#define MOD(n) comp->intrinsics . n = strings->intern(Axle::lit_view_arr(#n));
  INTRINSIC_MODS;
#undef MOD

  //Other important names
#define MOD(n) comp->important_names . n = strings->intern(Axle::lit_view_arr(#n));
  IMPORTANT_NAMES_INC;
#undef MOD

  comp->print_options = options.print;
  comp->optimization_options = options.optimize;

  //File stuff
  if (options.build.current_directory.size == 0) {
    comp_thread->report_error(ERROR_CODE::UNFOUND_DEPENDENCY, Span{},
                              "Current directory not specified");
    return;
  }

  {
    Axle::OwnedArr cwd = normalize_path(options.build.current_directory);

    if (!Axle::FILES::exists(view_arr(cwd))) {
      comp_thread->report_error(ERROR_CODE::UNFOUND_DEPENDENCY, Span{},
                                "Current directory was invalid: {}", cwd);
      return;
    }

    file_loader->cwd.directory = strings->intern(cwd);
  }

  comp->build_options.debug_break_on_entry = options.build.debug_break_on_entry;

  if (options.build.file_name.size == 0) {
    comp_thread->report_error(ERROR_CODE::UNFOUND_DEPENDENCY, Span{},
                              "Expected input file name");
    return;
  }

  comp->build_options.file_name = strings->intern(options.build.file_name);

  if (options.build.library) {
    if (options.build.entry_point.size == 0) {
      comp_thread->report_error(ERROR_CODE::LINK_ERROR, Span{},
                                "Cannot have an entry point and be a library (This is temporary)");
      return;
    }

    comp->build_options.is_library = true;
    comp->build_options.entry_point = nullptr;
  }
  else {
    if (options.build.entry_point.size == 0) {
      comp_thread->report_error(ERROR_CODE::UNFOUND_DEPENDENCY, Span{},
                                "Expected entry point");
      return;
    }

    comp->build_options.is_library = false;
    comp->build_options.entry_point = strings->intern(options.build.entry_point);
  }

  if (options.build.output_name.size == 0) {
    comp_thread->report_error(ERROR_CODE::UNFOUND_DEPENDENCY, Span{},
                              "Expected output file name");
    return;

  }

  comp->build_options.output_name = strings->intern(options.build.output_name);

  if (options.build.output_folder.size == 0) {
    comp_thread->report_error(ERROR_CODE::UNFOUND_DEPENDENCY, Span{},
                              "Expected output folder name");
    return;

  }

  comp->build_options.output_folder = strings->intern(options.build.output_folder);


  if (options.build.std_lib_folder.size == 0) {
    comp_thread->report_error(ERROR_CODE::UNFOUND_DEPENDENCY, Span{},
                              "Expected std lib folder");
    return;
  }

  {
    Axle::OwnedArr stdlib = normalize_path(view_arr(file_loader->cwd.directory), options.build.std_lib_folder);
    if (!Axle::FILES::exists(view_arr(stdlib))) {
      comp_thread->report_error(ERROR_CODE::UNFOUND_DEPENDENCY, Span{},
                                "std lib folder was invalid: {}", stdlib);
      return;
    }

    comp->build_options.std_lib_folder = strings->intern(stdlib);
  }

  if (options.build.lib_folder.size == 0) {
    comp_thread->report_error(ERROR_CODE::UNFOUND_DEPENDENCY, Span{},
                              "Expected lib folder");
    return;
  }

  {
    Axle::OwnedArr lib_folder = normalize_path(view_arr(file_loader->cwd.directory), options.build.lib_folder);
    if (!Axle::FILES::exists(view_arr(lib_folder))) {
      comp_thread->report_error(ERROR_CODE::UNFOUND_DEPENDENCY, Span{},
                                "lib folder was invalid: {}", lib_folder);
      return;
    }

    comp->build_options.lib_folder = strings->intern(lib_folder);
  }

  {
    const auto list = comp->platform_interface.valid_calling_conventions;
    const auto num = comp->platform_interface.num_calling_conventions;

    if (options.build.default_calling_convention >= num) {
      Format::ArrayFormatter error_message = {};
      Format::format_to(error_message, "\"[{}]\" was not a valid calling convention for system \"{}\"\n",
                      options.build.default_calling_convention,
                      comp->platform_interface.system_name);

      if (num > 0) {
        Format::format_to(error_message, "{} options are available:", num);

        for (usize i = 0; i < num; ++i) {
          const CallingConvention* cc = list[i];
          ASSERT(cc != nullptr);
          Format::format_to(error_message, "\n[{}] = \"{}\"", i, cc->name);
        }
      }
      else {
        Format::format_to(error_message, "No calling conventions available");
      }

      comp_thread->report_error(ERROR_CODE::UNFOUND_DEPENDENCY, Span{}, error_message.take());
      return;
    }
    else {
      comp->build_options.default_calling_convention = list[options.build.default_calling_convention];
    }
  }
}
