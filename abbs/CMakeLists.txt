cmake_minimum_required(VERSION 2.4)

if(COMMAND cmake_policy)
    cmake_policy(SET CMP0003 NEW)
endif(COMMAND cmake_policy)

set(CMAKE_ALLOW_LOOSE_LOOP_CONSTRUCTS ON)


project(ABBS)

if(NOT DEFINED BUILD_SHARED_LIBS)
    set(BUILD_SHARED_LIBS ON)
    MESSAGE(STATUS "** will build shared libraries by default")
    MESSAGE(STATUS "** use -DBUILD_SHARED_LIBS=OFF to build static libraries")
endif()

add_definitions(-Wall)

set(libbbs_SOURCES
    apdb.c
    apdb.h
    board.c
    env.c
    env.h
    util.c
    util.h)

set(libjail_SOURCES jail.c)

set(bbs_SOURCES bbs.c)

set(boardd_SOURCES boardd.c)

add_library(libbbs ${libbbs_SOURCES})
set_target_properties(libbbs PROPERTIES OUTPUT_NAME bbs)
set_target_properties(libbbs PROPERTIES VERSION 0.0 SOVERSION 0)

add_library(libjail SHARED ${libjail_SOURCES})
set_target_properties(libjail PROPERTIES OUTPUT_NAME jail)
target_link_libraries(libjail dl)

add_executable(bbs ${bbs_SOURCES})
target_link_libraries(bbs libbbs readline)

add_executable(boardd ${boardd_SOURCES})
target_link_libraries(boardd libbbs ev)

