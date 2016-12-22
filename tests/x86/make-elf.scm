;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2011, 2016 Göran Weinholt <goran@weinholt.se>

;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and associated documentation files (the "Software"),
;; to deal in the Software without restriction, including without limitation
;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;; and/or sell copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.
#!r6rs

;; This library generates a Linux x86 ELF image with the specified
;; text and data.

(library (zabavno tests x86 make-elf)
  (export make-x86-elf-image)
  (import (rnrs (6))
          (machine-code assembler x86)
          (machine-code assembler elf)
          (machine-code format elf))

  (define segments '(text data))

  (define sections (make-string-table '("" ".text" ".data" ".shstrtab" ".strtab" ".symtab")))

  (define (elf32-header entry)
    (define ph-start (vector))
    `((%label _elf_start)
      ,@(elf-32-assembler (make-elf-image
                           #f ELFCLASS32 ELFDATA2LSB ELFOSABI-SYSV 0
                           ET-EXEC EM-386 EV-CURRENT
                           entry
                           (if (null? segments) 0 '(- _elf_ph_start _elf_start))
                           (if (string-table-empty? sections) 0 '(- _elf_sh_start _elf_start))
                           0 #f #f (length segments) #f (string-table-size sections)
                           (or (string-table-list-index sections ".shstrtab")
                               SHN-UNDEF)))
      ;; Program header for text
      (%label _elf_ph_start)
      ,@(elf-32-assembler (make-elf-segment PT-LOAD (bitwise-ior PF-R PF-X)
                                            0 '_elf_start '_elf_start
                                            '(- _fini _elf_start)
                                            '(- _fini _elf_start)
                                            (expt 2 12)))
      ;; Program header for data and bss
      ,@(elf-32-assembler (make-elf-segment PT-LOAD (bitwise-ior PF-R PF-W)
                                            '(- _fini _elf_start)
                                            'data_start 'data_start
                                            '(- _edata data_start) '(- _end data_start)
                                            (expt 2 12)))
      ;; Section headers
      (%label _elf_sh_start)
      ,@(elf-32-assembler (make-elf-section 0 SHT-NULL 0 0 0 0 0 0 0 0))
      ,@(elf-32-assembler (make-elf-section
                           (string-table-byte-index sections ".text")
                           SHT-PROGBITS
                           (bitwise-ior SHF-EXECINSTR SHF-ALLOC)
                           'text '(- text _elf_start) '(- _fini text)
                           0 0 0 #f))
      ,@(elf-32-assembler (make-elf-section
                           (string-table-byte-index sections ".data")
                           SHT-PROGBITS
                           (bitwise-ior SHF-WRITE SHF-ALLOC)
                           'data_start '(- _fini _elf_start) '(- _edata data_start)
                           0 0 0 #f))
      ,@(elf-32-assembler (make-elf-section
                           (string-table-byte-index sections ".shstrtab")
                           SHT-STRTAB
                           0 0 '(- _shstrtab_start _elf_start)
                           '(- _shstrtab_end _shstrtab_start)
                           0 0 0 #f))
      ,@(elf-32-assembler (make-elf-section
                           (string-table-byte-index sections ".strtab")
                           SHT-STRTAB
                           0 0 '(- _strtab_start _elf_start)
                           '(- _strtab_end _strtab_start)
                           0 0 0 #f))
      ,@(elf-32-assembler (make-elf-section
                           (string-table-byte-index sections ".symtab")
                           SHT-SYMTAB
                           0 0 '(- _symtab_start _elf_start)
                           '(- _symtab_end _symtab_start)
                           (string-table-list-index sections ".strtab")
                           0 0 #f))))

  (define (make-program text data)
    (define wrapped-text
      `((%mode 32)
        (%origin #x08048000)
        ,@(elf32-header 'start)
        (%align 8 0)
        (%section text)
        (%label text)
        (%label start _fini global func)
        ,@text
        (%label exit)
        (mov ebx 0)                         ;status
        (mov eax 1)                         ;exit
        (int #x80)
        (%label _fini)))
    (define wrapped-data
      `((%call ,(lambda x 4096))            ;.text and .data share a page
        (%section data)
        (%label data_start)
        ,@data
        (%label _edata)

        ;; This does not take up space in the file, but will be
        ;; allocated zeroed memory when the program is loaded.
        (%align 16 0)
        (%label bss)
        (%section bss)
        (%label _end)
        ;; What follows does not get loaded into memory, but takes up
        ;; space in the file.

        (%section elf-stuff)
        (%call ,(lambda (assemble! port ip symbols bss _end)
                  (- -4096 (- _end bss)))
               bss _end)                    ;rewind back to .text
        (%label _shstrtab_start)
        (%vu8 ,(string-table-bytes sections))
        (%label _shstrtab_end)

        ;; Generating the symbol table properly would involve using a
        ;; %call and then outputting strings and symbols with `assemble!'.
        ;; The data from %label (with at least two arguments) and %comm is
        ;; available in the `symbols' argument.
        (%align 8 0)
        (%label _strtab_start)
        (%utf8z "")
        (%label _strtab_start_start)
        (%utf8z "start")
        (%label _strtab_end)

        (%align 4 0)
        (%label _symtab_start)
        ,@(elf-32-assembler (make-elf-symbol 0 0 0 0 0 0 0))
        ,@(elf-32-assembler
           (make-elf-symbol '(- _strtab_start_start _strtab_start)
                            STB-GLOBAL STT-FUNC 0
                            (string-table-list-index sections ".text")
                            'start '_fini))
        (%label _symtab_end)))
    (values wrapped-text wrapped-data))

  (define (make-x86-elf-image filename text data)
    (call-with-port (open-file-output-port filename)
      (lambda (p)
        (let*-values (((text data) (make-program text data))
                      ((machine-code symbol-table) (assemble (append text data))))
          (put-bytevector p machine-code)
          (close-port p))))))
