#include "backends.h"
#include "PE_file_format.h"

u8 Backend::DataBucketIterator::read_byte() {
  if (bucket_counter == BUCKET_SIZE) {
    bucket = bucket->next;
    bucket_counter = 0;
  }

  u8 b = bucket->arr[bucket_counter];
  bucket_counter += 1;
  actual_location += 1;
  return b;
}

void Backend::DataBucketIterator::jump_to(usize l) {
  ASSERT(l >= actual_location);
  while (actual_location < l) {
    ASSERT(bucket != nullptr);

    usize distance = l - actual_location;
    usize bucket_remainng = BUCKET_SIZE - bucket_counter;
    if (distance <= bucket_remainng) {
      bucket_counter += distance;
      actual_location += distance;
      return;
    }
    else {
      bucket = bucket->next;
      bucket_counter = 0;
      actual_location += bucket_remainng;
    }
  }
  ASSERT(l == actual_location);
}

void Backend::DataBucketIterator::overwrite_arr(const u8* arr, const u8* end) {
  while (true) {
    usize remaining = end - arr;

    if (BUCKET_SIZE - bucket_counter < remaining) {
      usize bucket_remaining = BUCKET_SIZE - bucket_counter;

      for (; bucket_counter < BUCKET_SIZE; ++bucket_counter) {
        bucket->arr[bucket_counter] = *arr;
        arr += 1;
      }

      actual_location += bucket_remaining;

      ASSERT(bucket->next != nullptr);
      bucket = bucket->next;
      bucket_counter = 0;
    }
    else {
      usize top_end = bucket_counter + remaining;
      for (; bucket_counter < top_end; ++bucket_counter) {
        bucket->arr[bucket_counter] = *arr;
        arr += 1;
      }

      actual_location += remaining;

      break;
    }
  }

  ASSERT(arr == end);
}

void Backend::DataBucketStore::free_held() {
  DataBucket* curr = bottom;

  while (curr != nullptr) {
    DataBucket* next = curr->next;
    free_destruct_single(curr);
    curr = next;
  }

  top_fill = 0;
  top = nullptr;
  bottom = nullptr;
  total_size = 0;
}


void Backend::DataBucketStore::push_zeros(usize count) {
  if (bottom == nullptr) {
    bottom = allocate_default<DataBucket>();
    top = bottom;
    top_fill = 0;
    total_size = 0;
  }

  while (BUCKET_SIZE - top_fill < count) {
    usize bucket_remaining = BUCKET_SIZE - top_fill;

    for (; top_fill < BUCKET_SIZE; ++top_fill) {
      top->arr[top_fill] = 0;
    }

    total_size += bucket_remaining;
    count -= bucket_remaining;

    DataBucket* new_b = allocate_default<DataBucket>();
    top->next = new_b;
    top = new_b;
    top_fill = 0;
  }
  
  usize top_end = top_fill + count;
  for (; top_fill < top_end; ++top_fill) {
    top->arr[top_fill] = 0;
  }

  total_size += count;
}

void Backend::DataBucketStore::push_arr(const u8* arr, const u8* end) {
  if (bottom == nullptr) {
    bottom = allocate_default<DataBucket>();
    top = bottom;
    top_fill = 0;
    total_size = 0;
  }

  while (true) {
    usize remaining = end - arr;

    if (BUCKET_SIZE - top_fill < remaining) {
      usize bucket_remaining = BUCKET_SIZE - top_fill;

      for (; top_fill < BUCKET_SIZE; ++top_fill) {
        top->arr[top_fill] = *arr;
        arr += 1;
      }

      total_size += bucket_remaining;

      DataBucket* new_b = allocate_default<DataBucket>();
      top->next = new_b;
      top = new_b;
      top_fill = 0;
    }
    else {
      usize top_end = top_fill + remaining;
      for (; top_fill < top_end; ++top_fill) {
        top->arr[top_fill] = *arr;
        arr += 1;
      }

      total_size += remaining;

      break;
    }
  }

  ASSERT(arr == end);
}