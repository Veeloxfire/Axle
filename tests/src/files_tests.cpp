#include "tests.h"
#include "files.h"

TEST_FUNCTION(Files, normalise_paths) {

  {
    constexpr char ARR[] = ".\\hello\\thing2";
    OwnedArr str = normalize_path("hello/world/thing/", "../../thing2/thing3/../");
    TEST_STR_EQ(ARR, str.data);
  }

  {
    constexpr char ARR[] = ".\\hello\\thing2";
    OwnedArr str = normalize_path("hello/world/thing", "../../thing2/thing3/../");
    TEST_STR_EQ(ARR, str.data);
  }

  {
    constexpr char ARR[] = ".\\hello\\thing2";
    OwnedArr str = normalize_path("hello/world/thing/", "../../thing2/thing3/..");
    TEST_STR_EQ(ARR, str.data);
  }

  {
    constexpr char ARR[] = ".\\hello\\thing2";
    OwnedArr str = normalize_path("hello/world/thing", "../../thing2/thing3/..");
    TEST_STR_EQ(ARR, str.data);
  }

  {
    constexpr char ARR[] = "C:\\hello\\thing2\\thing3";
    OwnedArr str = normalize_path("C:\\hello/world/thing", "../../thing2/thing3");
    TEST_STR_EQ(ARR, str.data);
  }

  {
#define DIRECTORY ".\\hello\\thing2\\thing3\\"
#define NAME "two"
#define EXTENSION "exe"
    constexpr char DIRECTORY_ARR[] = DIRECTORY;
    constexpr char NAME_ARR[] = NAME;
    constexpr char EXTENSION_ARR[] = EXTENSION;
    constexpr char ARR[] = DIRECTORY NAME "." EXTENSION;
#undef DIRECTORY
#undef NAME
#undef EXTENSION

    AllocFilePath str = format_file_path("hello/world/thing", "../../thing2/thing3/two", "exe");
    TEST_STR_EQ(ARR, str.raw.data);

    const char* dir = str.raw.data;
    TEST_ARR_EQ(DIRECTORY_ARR, sizeof(DIRECTORY_ARR) - 1, dir, str.directory_size);

    const char* name = str.raw.data + str.file_name_start;
    TEST_ARR_EQ(NAME_ARR, sizeof(NAME_ARR) - 1, name, str.file_name_size);

    const char* ext = str.raw.data + str.extension_start;
    TEST_ARR_EQ(EXTENSION_ARR, sizeof(EXTENSION_ARR) - 1, ext, str.extension_size);
  }

}