#!/bin/sh

rm /home/mbc/temp/mylib/deposit/*.*
cp /home/mbc/projects/bookstore/samples/txt/*.* /home/mbc/temp/mylib/deposit

mc rm --recursive --force myminio/bookstore
mc cp /home/mbc/projects/bookstore/samples/jsons/*.* myminio/bookstore
