#include "unit_tests.h"
#include "files.h"

TEST_FUNCTION(Files, normalise_paths) {
  {
    constexpr ViewArr<const char> ARR = lit_view_arr("C:\\hello\\thing2\\thing3");
    OwnedArr str = normalize_path(lit_view_arr("C:\\hello/world/thing/../../thing2/thing3"));
    TEST_STR_EQ(ARR, str);
  }

  {
    constexpr ViewArr<const char> ARR = lit_view_arr(".\\hello\\thing2");
    OwnedArr str = normalize_path(lit_view_arr("hello/world/thing/"), lit_view_arr("../../thing2/thing3/../"));
    TEST_STR_EQ(ARR, str);
  }

  {
    constexpr ViewArr<const char> ARR = lit_view_arr(".\\hello\\thing2");
    OwnedArr str = normalize_path(lit_view_arr("hello/world/thing"), lit_view_arr("../../thing2/thing3/../"));
    TEST_STR_EQ(ARR, str);
  }

  {
    constexpr ViewArr<const char> ARR = lit_view_arr(".\\hello\\thing2");
    OwnedArr str = normalize_path(lit_view_arr("hello/world/thing/"), lit_view_arr("../../thing2/thing3/.."));
    TEST_STR_EQ(ARR, str);
  }

  {
    constexpr ViewArr<const char> ARR = lit_view_arr(".\\hello\\thing2");
    OwnedArr str = normalize_path(lit_view_arr("hello/world/thing"), lit_view_arr("../../thing2/thing3/.."));
    TEST_STR_EQ(ARR, str);
  }

  {
    constexpr ViewArr<const char> ARR = lit_view_arr("C:\\hello\\thing2\\thing3");
    OwnedArr str = normalize_path(lit_view_arr("C:\\hello/world/thing"), lit_view_arr("../../thing2/thing3"));
    TEST_STR_EQ(ARR, str);
  }

  {
#define DIRECTORY ".\\hello\\thing2\\thing3\\"
#define NAME "two"
#define EXTENSION "exe"
    constexpr auto DIRECTORY_ARR = lit_view_arr(DIRECTORY);
    constexpr auto NAME_ARR = lit_view_arr(NAME);
    constexpr auto EXTENSION_ARR = lit_view_arr(EXTENSION);
    constexpr auto ARR = lit_view_arr(DIRECTORY NAME "." EXTENSION);
#undef DIRECTORY
#undef NAME
#undef EXTENSION

    {
      AllocFilePath str = format_file_path(lit_view_arr("hello/world/thing"),
                                           lit_view_arr("../../thing2/thing3/two"),
                                           lit_view_arr("exe"));

      TEST_STR_EQ(ARR, str.raw);
      TEST_STR_EQ(DIRECTORY_ARR, view_arr(str.raw, 0, str.directory_size));
      TEST_STR_EQ(NAME_ARR, view_arr(str.raw, str.file_name_start, str.file_name_size));
      TEST_STR_EQ(EXTENSION_ARR, view_arr(str.raw, str.extension_start, str.extension_size));
    }

    {
      AllocFilePath str = format_file_path(lit_view_arr("hello/world/thing"),
                                           lit_view_arr("../../thing2/thing3/two.exe"));

      TEST_STR_EQ(ARR, str.raw);
      TEST_STR_EQ(DIRECTORY_ARR, view_arr(str.raw, 0, str.directory_size));
      TEST_STR_EQ(NAME_ARR, view_arr(str.raw, str.file_name_start, str.file_name_size));
      TEST_STR_EQ(EXTENSION_ARR, view_arr(str.raw, str.extension_start, str.extension_size));
    }
  }
}


TEST_FUNCTION(Files, parse_file_locations) {
#define DIRECTORY ".\\hello\\thing2\\thing3\\"
#define NAME "two"
#define EXTENSION "exe"
  constexpr auto DIRECTORY_ARR = lit_view_arr(DIRECTORY);
  constexpr auto NAME_ARR = lit_view_arr(NAME);
  constexpr auto EXTENSION_ARR = lit_view_arr(EXTENSION);
  constexpr auto ARR = lit_view_arr(DIRECTORY NAME "." EXTENSION);
#undef DIRECTORY
#undef NAME
#undef EXTENSION

  {
    StringInterner strings = {};

    AllocFilePath str = format_file_path(lit_view_arr("hello/world/thing"),
                                         lit_view_arr("../../thing2/thing3/two"),
                                         lit_view_arr("exe"));

    TEST_STR_EQ(ARR, str.raw);
    TEST_STR_EQ(DIRECTORY_ARR, view_arr(str.raw, 0, str.directory_size));
    TEST_STR_EQ(NAME_ARR, view_arr(str.raw, str.file_name_start, str.file_name_size));
    TEST_STR_EQ(EXTENSION_ARR, view_arr(str.raw, str.extension_start, str.extension_size));

    FileLocation files = parse_file_location(str, &strings);

    const InternString* full = strings.intern(ARR);
    const InternString* dir = strings.intern(DIRECTORY_ARR);
    const InternString* ext = strings.intern(EXTENSION_ARR);

    TEST_EQ(full, files.full_name);
    TEST_EQ(dir, files.directory);
    TEST_EQ(ext, files.extension);
  }

  {
    StringInterner strings = {};

    FileLocation files = parse_file_location(lit_view_arr("hello/world/thing"),
                                             lit_view_arr("../../thing2/thing3/two.exe"), &strings);

    const InternString* full = strings.intern(ARR);
    const InternString* dir = strings.intern(DIRECTORY_ARR);
    const InternString* ext = strings.intern(EXTENSION_ARR);

    TEST_EQ(full, files.full_name);
    TEST_EQ(dir, files.directory);
    TEST_EQ(ext, files.extension);
  }
}