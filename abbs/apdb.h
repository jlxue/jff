/*
 * apdb - an append-only record database
 *        
 * Copyright (C) 2008 Liu Yubao <yubao.liu@gmail.com>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 */

#ifndef APDB_H__
#define APDB_H__

#include <stdint.h>
#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif


typedef struct apdb_s apdb_t;

typedef off_t apdb_record_t;


/**
 * open a database to read or write
 */
apdb_t*
apdb_open(const char* path, char mode, size_t index_content_len);


/**
 * close a database
 */
int
apdb_close(apdb_t* db);


/**
 * begin to add a record
 */
int
apdb_add_begin(apdb_t* db, size_t length);


/**
 * append data to the last record in data file
 */
int
apdb_append_data(apdb_t* db, const void* content, size_t length);


/**
 * finish adding a record
 */
apdb_record_t
apdb_add_end(apdb_t* db, const void* content);


/**
 * get a record by id
 */
apdb_record_t
apdb_get(apdb_t* db, unsigned id);


/**
 * delete a record
 */
void
apdb_delete(apdb_t* db, apdb_record_t record);


/**
 * begin to update a record
 */
int
apdb_update_begin(apdb_t* db, apdb_record_t record, size_t length);


/**
 * finish updating a record
 */
int
apdb_update_end(apdb_t* db, void* content);


/**
 * update only index for a record
 */
int
apdb_update_index(apdb_t* db, apdb_record_t record, void* content);


/**
 * get count of all records in a database
 */
unsigned
apdb_count(apdb_t* db);


/**
 * get first record
 */
apdb_record_t
apdb_first(apdb_t* db);


/**
 * get last record
 */
apdb_record_t
apdb_last(apdb_t* db);


/**
 * get next record
 */
apdb_record_t
apdb_next(apdb_t* db, apdb_record_t record);


/**
 * get previous record
 */
apdb_record_t
apdb_previous(apdb_t* db, apdb_record_t record);


/**
 * get the id of a record
 */
unsigned
apdb_record_id(apdb_t* db, apdb_record_t record);


/**
 * get the flags of a record
 */
unsigned
apdb_record_flags(apdb_t* db, apdb_record_t record);


/**
 * get the offset of a record in data file
 */
off_t
apdb_record_offset(apdb_t* db, apdb_record_t record);


/**
 * get the length of a record
 */
size_t
apdb_record_length(apdb_t* db, apdb_record_t record);


/**
 * get the data of a record
 */
void*
apdb_record_data(apdb_t* db, apdb_record_t record);


/**
 * get the index of a record
 */
void*
apdb_record_index(apdb_t* db, apdb_record_t record);


#ifdef __cplusplus
}
#endif

#endif /* APDB_H__ */

