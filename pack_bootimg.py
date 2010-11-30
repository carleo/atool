#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys
import os
import shutil
from struct import pack,unpack,calcsize
from optparse import OptionParser

PAGESIZE = 2048

PRODUCT_MAP = {'32a':('no_console_suspend=1 console=null quiet board_trout.disable_uart3=1', '0x19200000'),
               '32b':('no_console_suspend=1 console=null quiet board_trout.disable_uart3=1', ''),
               }

def dump_file(infh, outfh, size):
    remain = size
    while remain > 0:
        data = infh.read(min(remain, PAGESIZE))
        if len(data) == 0:
            return False
        outfh.write(data)
        remain -= len(data)
    if size % PAGESIZE:
        remain = PAGESIZE - (size % PAGESIZE)
        while remain > 0:
            data = infh.read(remain)
            if len(data) == 0:
                return False
            remain -= len(data)
    return True

def unpack_image(filename, extractramdisk):
    fh = open(filename, "r")
    header = fh.read(PAGESIZE)
    assert len(header) == PAGESIZE, "incomplete header"
    assert header.find("ANDROID!") == 0, "invalid header(magic unmatch)"
    (kernel_size, kernel_addr, ramdisk_size, ramdisk_addr,
     second_size, second_addr, tags_addr, page_size,
     unused0, unused1) = unpack('<iiiiiiiiii', header[8:48])

    imgname = os.path.basename(filename)
    if imgname.endswith(".img") and len(imgname) > 4:
        imgname = imgname[0:-4]

    outfh = open(imgname + "-kernel", "w")
    if not dump_file(fh, outfh, kernel_size):
        print "incomplete kernel part"
        sys.exit(1)
    outfh.close()
    
    outfh = open(imgname + "-ramdisk.cpio.gz", "w")
    if not dump_file(fh, outfh, ramdisk_size):
        print "incomplete ramdisk part"
        sys.exit(1)
    outfh.close()

    if second_size > 0:
        outfh = open(imgname + "-second", "w")
        if not dump_file(fh, outfh, second_size):
            print "incomplete second part"
            sys.exit(1)
        outfh.close()

    fh.close()

    if extractramdisk:
        ramdir = imgname + "-ramdisk"
        if os.path.exists(ramdir):
            shutil.rmtree(ramdir)
            print "remove old ramdisk directory " + ramdir

        os.mkdir(ramdir)
        os.chdir(ramdir)
        os.system("gunzip -c ../%s-ramdisk.cpio.gz | cpio -i" % (imgname))
        os.unlink("../%s-ramdisk.cpio.gz" % (imgname))
        print "extract ramdisk contents to directory " + imgname + "-ramdisk/"


def repack_image(kernel, ramdisk, imgfile, product):
    if not os.path.isfile(kernel):
        print "kernel '%s' is not a regular file" % (kernel)
        sys.exit(1)
    if not os.path.exists(ramdisk):
        print "ramdisk '%s' not exists"
        sys.exit(1)
    ramname = ramdisk
    if os.path.isdir(ramdisk):
        olddir = os.getcwd()
        os.chdir(ramdisk)
        ramname = "%s/ramdisk-repack.cpio.gz" % (olddir)
        os.system("find . | cpio -o -H newc | gzip > '%s'" % (ramname))
        os.chdir(olddir)

    cmdline = PRODUCT_MAP[product][0]
    baseaddr = PRODUCT_MAP[product][1]
    if baseaddr:
        baseaddr = "--base 0x19200000"
    os.system("mkbootimg --cmdline '%s' %s --kernel '%s' --ramdisk '%s' -o '%s'" % (cmdline, baseaddr, kernel, ramname, imgfile))

    if ramname != ramdisk:
        os.unlink(ramname)
    print "repack image to " + imgfile

if __name__ == "__main__":
    parser = OptionParser()
    parser.add_option("-p", "--product", dest="product", type="string",
                      help="product, can be one of %s" % (",".join(PRODUCT_MAP.keys())))
    parser.add_option("-i", "--image", dest="image", type="string",
                      help="image file")
    parser.add_option("-k", "--kernel", dest="kernel", type="string",
                      help="kernel file")
    parser.add_option("-r", "--ramdisk", dest="ramdisk", type="string",
                      help="ramdisk")
    parser.add_option("-x", "--extract", dest="extract", action="store_true",
                      default=False, help="extract ramdisk")
    # parser.add_option("-s", "--second", dest="second", type="string",
    #                   help="second file")
    parser.add_option("-u", "--unpack", dest="unpack", action="store_true",
                      default=False, help="unpack image file")
    (options, args) = parser.parse_args()
    if not options.image:
        print "error: no image file specified"
        sys.exit(1)
    if options.unpack:
        unpack_image(options.image, options.extract)
    else:
        if not options.product:
            product = "32a"
        else:
            product = options.product
        if product not in PRODUCT_MAP:
            print "product should be one of %s" % (",".join(PRODUCT_MAP.keys()))
            sys.exit(1)
        if not options.kernel or not options.ramdisk:
            print "error: both kernel and ramdisk must be specified"
            sys.exit(1)
        else:
            repack_image(options.kernel, options.ramdisk, options.image, product)
        
