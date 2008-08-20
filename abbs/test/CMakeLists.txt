include_directories(${CMAKE_SOURCE_DIR})

set(test_apdb_SOURCES test_apdb.c)

add_executable(test_apdb ${test_apdb_SOURCES})
target_link_libraries(test_apdb libbbs)

