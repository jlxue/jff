/*
 * apdb - append-only record database
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
 *
 */

#ifndef APDB_H__
#define APDB_H__

#include <stdint.h>
#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif


#define DELETED             0x1
#define WRITING             0x2


typedef struct apdb_s apdb_t;

typedef off_t apdb_record_t;


/**
 * open a database to read or write
 *
 * @param path  path to database file without suffix
 * @param mode  'r' to read only, 'w' to read and write
 * @param index_content_len length of extra data contained in an index item
 *
 * @return NULL on error, pointer to a database instance on success
 */
apdb_t*
apdb_open(const char* path, char mode, size_t index_content_len);


/**
 * close a database
 *
 * @param db    ponter to a database instance
 *
 * @return 0 on success, -1 on error
 */
int
apdb_close(apdb_t* db);


/**
 * begin to add a new record
 *
 * @param db    pointer to a database instance
 * @param length    length of the new record, @c apdb_append_data will be used
 *                  to provide data
 *
 * @return -1 on error, id of the new record on success
 *
 * @remarks A database can contains UINT_MAX-1 records at most.
 */
unsigned
apdb_add_begin(apdb_t* db, size_t length);


/**
 * append data to the last record in data file
 *
 * @param db    pointer to a database instance
 * @param content   data for the latest added/updated record
 * @param length    length of content
 *
 * @return -1 on error, 0 on success
 *
 * @remarks This function can be called many times to provide data for
 * a record and the total length of these data must be equal to the
 * length argument of @c apdb_add_begin or @c apdb_update_begin.
 *
 * @remarks The call sequences @c add_begin->append_data->add_end and
 * @c update_begin->append_data->update_end mustn't be interrupted or
 * intersected.
 */
int
apdb_append_data(apdb_t* db, const void* content, size_t length);


/**
 * finish adding a record
 *
 * @param db    pointer to a database instance
 * @param content   extra data that will be contained in the index item of
 *                  a record, length of @c content must be equal to the
 *                  @c index_content_len argument of @c apdb_open
 * 
 * @return  -1 on error, or the new record on success
 */
apdb_record_t
apdb_add_end(apdb_t* db, const void* content);


/**
 * get a record by id
 *
 * @param db    pointer to a database instance
 * @param id    id of the record to search
 * 
 * @return -1 if not found or something goes wrong, or
 * the corresponding record on success
 */
apdb_record_t
apdb_get(apdb_t* db, unsigned id);


/**
 * delete a record
 *
 * @param db    pointer to a database instance
 * @param record the record to be deleted
 */
void
apdb_delete(apdb_t* db, apdb_record_t record);


/**
 * delete many records
 *
 * @param db    pointer to a database instance
 * @param from  the beginning record
 * @param to    the ending record, included.
 *
 * @return count of deleted records
 */
unsigned
apdb_delete_range(apdb_t* db, apdb_record_t from, apdb_record_t to);


/**
 * begin to update a record
 *
 * @param db    pointer to a database instance
 * @param record the record to be updated
 * @param length length of the new record, @c apdb_append_data will be used
 *               to provide data
 *
 * @return -1 on error, 0 on success
 */
int
apdb_update_begin(apdb_t* db, apdb_record_t record, size_t length);


/**
 * finish updating a record
 *
 * @param db    pointer to a database instance
 * @param content   extra data that will be contained in the index item of
 *                  a record, length of @c content must be equal to the
 *                  @c index_content_len argument of @c apdb_open
 *
 * @return -1 on error, 0 on success
 *
 * @remarks If @c content is @c NULL, the extra data in the index item will
 * not be changed.
 */
int
apdb_update_end(apdb_t* db, void* content);


/**
 * get count of all records in a database
 *
 * @param db    pointer to a database instance
 * 
 * @return total count of records in the database, including deleted
 * records as deleted records aren't really deleted in an append-only
 * database.
 *
 * @remarks To avoid callers check return value, this function returns 0
 * on error.
 *
 * @note Use apdb_first/last/next/previous and apdb_foreach to iterate all
 * records and count undeleted records.
 */
unsigned
apdb_count(apdb_t* db);


/**
 * get first record
 *
 * @param db    pointer to a database instance
 *
 * @return -1 on error or no record in the database, the first record on
 * success.
 */
apdb_record_t
apdb_first(apdb_t* db);


/**
 * get last record
 *
 * @param db    pointer to a database instance
 *
 * @return -1 on error or no record in the database, the last record on
 * success.
 */
apdb_record_t
apdb_last(apdb_t* db);


/**
 * get next record
 *
 * @param db    pointer to a database instance
 * @param record the current record
 *
 * @return -1 on error or no record in the database, the next record on
 * success.
 */
apdb_record_t
apdb_next(apdb_t* db, apdb_record_t record);


/**
 * get previous record
 *
 * @param db    pointer to a database instance
 * @param record the current record
 *
 * @return -1 on error or no record in the database, the previous record on
 * success.
 */
apdb_record_t
apdb_previous(apdb_t* db, apdb_record_t record);


/**
 * iterate records
 *
 * @param db    pointer to a database instance
 * @param from  the starting record
 * @param to    the ending record, included.
 * @param count maximum count of records to be iterated.
 * @param op    function to be called on each record when iterate, return
 *              non-zero to interrupt the iteration.
 * @param arg   extra argument of @c op
 *
 * @return -1 on error, count of iterated records on success.
 *
 * @remarks If @c db is opened to read and write, @c *flags and @c *index can
 * be modified directly to modify index item of a record, or else they are
 * read-only.
 *
 * @remarks If @c *index is modified directly, be very careful to prevent
 * reader processes from getting wrong data and make sure inconsistent data
 * not harmful. It's a good idea to set WRITING flag in @c *flags but there
 * exists race condition. Make sure the last characters of string fields in
 * @c *index are always '\\0' when modify @c *index.
 */
unsigned
apdb_foreach(apdb_t* db, apdb_record_t from, apdb_record_t to, unsigned count,
             int (*op)(apdb_record_t record, unsigned id, unsigned* flags,
                       size_t length, void* index, void* arg),
             void* arg);


/**
 * get the id of a record
 *
 * @param db    pointer to a database instance
 * @param record a record
 *
 * @return id of the record
 */
unsigned
apdb_record_id(apdb_t* db, apdb_record_t record);


/**
 * get the flags of a record
 *
 * @param db    pointer to a database instance
 * @param record a record
 *
 * @return flags of the record
 */
unsigned
apdb_record_flags(apdb_t* db, apdb_record_t record);


/**
 * get the length of a record
 *
 * @param db    pointer to a database instance
 * @param record a record
 *
 * @return length of the record
 */
size_t
apdb_record_length(apdb_t* db, apdb_record_t record);


/**
 * get the data of a record
 *
 * @param db    pointer to a database instance
 * @param record a record
 *
 * @return content of the record
 */
const void*
apdb_record_data(apdb_t* db, apdb_record_t record);


/**
 * get the index of a record
 *
 * @param db    pointer to a database instance
 * @param record a record
 *
 * @return extra data contented in the index item of the record
 */
void*
apdb_record_index(apdb_t* db, apdb_record_t record);


/**
 * update flags for a record
 *
 * @param db    pointer to a database instance
 * @param record a record
 * @param flags new flags
 *
 * @return -1 on error, 0 on success
 */
int
apdb_record_set_flags(apdb_t* db, apdb_record_t record, unsigned flags);


/**
 * set WRITING flag
 *
 * @param db    pointer to a database instance
 * @param record a record
 *
 * @return -1 on error, 0 on success
 */
int
apdb_record_lock(apdb_t* db, apdb_record_t record);


/**
 * clear WRITING flag
 *
 * @param db    pointer to a database instance
 * @param record a record
 *
 * @return -1 on error, 0 on success
 */
void
apdb_record_unlock(apdb_t* db, apdb_record_t record);


#ifdef __cplusplus
}
#endif

#endif /* APDB_H__ */

