; SD Card and FAT32 development testbed
; contains reusable functions that can be the basis for enhancing the monitor
; done by sy2002 in June 2016

#include "../dist_kit/sysdef.asm"
#include "../dist_kit/monitor.def"

                .ORG 0x8000

                MOVE    STR_TITLE, R8
                SYSCALL(puts, 1)

                ; Reset SD Card
                MOVE    STR_RESET, R8
                SYSCALL(puts, 1)
                RSUB    SD$RESET, 1
                RSUB    ERR_CHECK, 1
                MOVE    STR_OK, R8
                SYSCALL(puts, 1)
                SYSCALL(crlf, 1)
                
                ; SD registers
                MOVE    IO$SD_ADDR_LO, R0
                MOVE    IO$SD_ADDR_HI, R1
                MOVE    IO$SD_DATA_POS, R2
                MOVE    IO$SD_DATA, R3
                MOVE    IO$SD_ERROR, R4
                MOVE    IO$SD_CSR, R5

                ; perform register write/read-back checks
                MOVE    STR_REGCHK_T, R8
                SYSCALL(puts, 1)
                MOVE    0, R11                  ; deactivate do not write mode
                MOVE    R0, R8                  ; check ADDR_LO
                MOVE    0x2309, R9
                MOVE    STR_REGCHK_AL, R10
                RSUB    REG_CHECK, 1
                MOVE    R1, R8                  ; check ADDR_HI
                MOVE    0xABA0, R9
                MOVE    STR_REGCHK_AH, R10
                RSUB    REG_CHECK, 1
                MOVE    R2, R8                  ; check DATA_POS
                MOVE    0x4505, R9
                MOVE    STR_REGCHK_DP, R10
                RSUB    REG_CHECK, 1
                MOVE    R3, R8                  ; check DATA
                MOVE    0x0076, R9              ; (is an 8-bit register)
                MOVE    STR_REGCHK_DTA, R10
                RSUB    REG_CHECK, 1
                MOVE    1, R11                  ; activate do not write mode
                MOVE    R4, R8                  ; ERROR is read only
                MOVE    0x0000, R9              ; and must be zero
                MOVE    STR_REGCHK_ER, R10
                RSUB    REG_CHECK, 1
                MOVE    R5, R8                  ; CSR status bits
                MOVE    0x0000, R9              ; must be zero
                MOVE    STR_REGCHK_CS, R10
                RSUB    REG_CHECK, 1

                ; main menu
MAIN_MENU       MOVE    STR_MEN_TITLE, R8
                SYSCALL(puts, 1)
MAIN_MRR        SYSCALL(getc, 1)
                CMP     R8, '1'
                RBRA    IA_DUMP, Z
                CMP     R8, '2'
                RBRA    MNT_SD, Z
                CMP     R8, '3'
                RBRA    END_PROGRAM, Z

                RBRA    MAIN_MRR, 1

;=============================================================================
; Mount partition 1 as FAT32 and perform various tests
;=============================================================================                

MNT_SD          MOVE    STR_MNT_TILE, R8        ; print testcase title
                SYSCALL(puts, 1)

                MOVE    DEVICE_HANDLE, R8       ; device handle
                MOVE    1, R9                   ; partition #1
                RSUB    FAT32$MOUNT_SD, 1       ; mount device
                MOVE    R9, R8
                RSUB    ERR_CHECK, 1
                MOVE    STR_OK, R8
                SYSCALL(puts, 1)
                SYSCALL(crlf, 1)

                MOVE    STR_MNT_FSSTART, R8     ; show fs start address
                MOVE    FAT32$DEV_FS_HI, R9
                MOVE    FAT32$DEV_FS_LO, R10
                MOVE    1, R11
                RSUB    OUTPUT_DW, 1

                MOVE    STR_MNT_FATSTRT, R8     ; show FAT start address
                MOVE    FAT32$DEV_FAT_HI, R9
                MOVE    FAT32$DEV_FAT_LO, R10
                RSUB    OUTPUT_DW, 1

                MOVE    STR_MNT_CLSSTRT, R8     ; show clusters start address
                MOVE    FAT32$DEV_CLUSTER_HI, R9
                MOVE    FAT32$DEV_CLUSTER_LO, R10
                RSUB    OUTPUT_DW, 1

                MOVE    STR_MNT_SPC, R8         ; show sectors per cluster
                MOVE    FAT32$DEV_SECT_PER_CLUS, R9
                MOVE    0, R11
                RSUB    OUTPUT_DW, 1

                MOVE    STR_MNT_ROOT, R8        ; show 1st cluster of root dir
                MOVE    FAT32$DEV_RD_1STCLUS_HI, R9
                MOVE    FAT32$DEV_RD_1STCLUS_LO, R10
                MOVE    1, R11
                RSUB    OUTPUT_DW, 1

                MOVE    STR_MNT_ACTIVE, R8      ; show 1st cluster active dir
                MOVE    FAT32$DEV_AD_1STCLUS_HI, R9
                MOVE    FAT32$DEV_AD_1STCLUS_LO, R10
                RSUB    OUTPUT_DW, 1

                SYSCALL(crlf, 1)
        
                ; list current directory                

                MOVE    DEVICE_HANDLE, R8       ; open directory for browsing
                SUB     FAT32$FDH_STRUCT_SIZE, SP
                MOVE    SP, R0                  ; R0 = directory handle
                MOVE    R0, R9
                RSUB    FAT32$DIR_OPEN, 1
                MOVE    R8, R0                  ; test: R8 should still be R0
                MOVE    R9, R8
                RSUB    ERR_CHECK, 1

                SUB     FAT32$DE_STRUCT_SIZE, SP
                MOVE    SP, R1                  ; R1 = directory entry struct
NEXT_DIR_ENT    MOVE    R0, R8
                MOVE    R1, R9
                MOVE    FAT32$FA_ALL, R10
                RSUB    FAT32$DIR_LIST, 1                
                MOVE    R11, R8
                RSUB    ERR_CHECK, 1
                CMP     R10, 1                  ; current entry valid?
                RBRA    END_PROGRAM, !Z         ; no: end

                MOVE    R1, R8                  ; yes: print entry
                MOVE    FAT32$PRINT_ALL, R9
                RSUB    FAT32$PRINT_DE, 1

                RBRA    NEXT_DIR_ENT, 1         ; next entry

                RBRA    END_PROGRAM, 1

;=============================================================================
; Interactive register dump
;=============================================================================

                ; allow interactive dumps of arbitrary addresses
IA_DUMP         MOVE    STR_IA_TITLE, R8
                SYSCALL(puts, 1)
                MOVE    STR_IA_HIGH, R8
                SYSCALL(puts, 1)
                SYSCALL(gethex, 1)
                MOVE    R8, R1                  ; block addr hi word
                SYSCALL(crlf, 1)
                MOVE    STR_IA_LOW, R8
                SYSCALL(puts, 1)
                SYSCALL(gethex, 1)
                MOVE    R8, R0                  ; block read addr lo word
                SYSCALL(crlf, 1)
                SYSCALL(crlf, 1)

                ; read 512 bytes from given address
                ; address must be a multiple of 512, otherwise the system
                ; will automatically "round it down" to the next 512 block
                MOVE    R0, R8
                MOVE    R1, R9
                RSUB    SD$READ_BLOCK, 1
                RSUB    ERR_CHECK, 1

                ; output the 512 bytes of the buffer
                MOVE    0, R6
OUTPUT_LOOP     MOVE    R6, R8                  ; read byte at position R6...
                RSUB    SD$READ_BYTE, 1         ; ...from buffer
                SYSCALL(puthex, 1)              ; output hex value
                MOVE    STR_SPACE2, R8          ; output two separating spaces
                SYSCALL(puts, 1)
                ADD     1, R6                   ; next byte
                MOVE    R6, R8                  ; if bytecount mod 16 is zero,
                MOVE    16, R9                  ; i.e. if a line has 16 hex
                SYSCALL(divu, 1)                ; numbers, then output a
                CMP     0, R11                  ; CR/LF so that the output
                RBRA    OUTPUT_LOOP_1, !Z       ; is nicely formatted
                SYSCALL(crlf, 1)
OUTPUT_LOOP_1   CMP     512, R6
                RBRA    OUTPUT_LOOP, !Z
                SYSCALL(crlf, 1)

                ; check if the user like to dump another buffer
                MOVE STR_IA_AGAIN, R8
                SYSCALL(puts, 1)
                SYSCALL(getc, 1)
                SYSCALL(crlf, 1)
                CMP     'y', R8
                RBRA    IA_DUMP, Z
                RBRA    MAIN_MENU, 1

                ; back to monitor
END_PROGRAM     SYSCALL(exit, 1)


;=============================================================================
; Helper sub routines for performing the checks
;=============================================================================

; Register check subroutine: Expects the to be written and read-back
; register in R8, the value in R9 and the name string in R10
; R11: if 1 then no write is performed, only the read-back. This is needed
; for the CSR register as writing to it performs an action. It is also
; advisable for the ERROR register as you cannot write to it
REG_CHECK       INCRB
                MOVE    R8, R0

                ; print "checking <register name>"
                MOVE    STR_REGCHK_R, R8
                SYSCALL(puts, 1)
                MOVE    R10, R8
                SYSCALL(puts, 1)

                CMP     R11, 1
                RBRA    _REG_CHECK_DW, Z

                ; write SD card register, read it back and test the value
                MOVE    R9, @R0                 ; write to the register
_REG_CHECK_DW   MOVE    @R0, R1                 ; read it back
                CMP     R1, R9                  ; check if the read val is ok
                RBRA    _REG_CHECK_OK, Z        ; jump if OK
                MOVE    STR_FAILED, R8          ; print FAILED, if not OK...
                SYSCALL(puts, 1)
                MOVE    R1, R8
                SYSCALL(puthex, 1)              ; ...and show the wrong value
                RBRA    _REG_CHECK_CNT, 1
_REG_CHECK_OK   MOVE    STR_OK, R8              ; print OK, if OK
                SYSCALL(puts, 1)
_REG_CHECK_CNT  SYSCALL(crlf, 1)

                DECRB
                RET

; Error check subroutine: If R8 is nonzero, then the error code is printed
; and then the program is terminated
ERR_CHECK       INCRB

                CMP     R8, 0                   ; if no error: return
                RBRA    _ERR_CHECK_END, Z

                MOVE    R8, R9                  ; save error code
                MOVE    STR_ERR_END, R8         ; print error string
                SYSCALL(puts, 1)
                MOVE    R9, R8                  ; print error code
                SYSCALL(puthex, 1)
                SYSCALL(crlf, 1)
                DECRB                           ; terminate execution
                SYSCALL(exit, 1)

_ERR_CHECK_END  DECRB
                RET

; Output a title and a DWORD value from the device handle
; R8 = Title
; R9 = offset lo
; R10 = offset hi
; R11: output DWORD, when R11 = 1, else output WORD
OUTPUT_DW       INCRB

                MOVE    R8, R0

                SYSCALL(puts, 1)

                MOVE    DEVICE_HANDLE, R8
                ADD     R9, R8
                MOVE    @R8, R8
                SYSCALL(puthex, 1)

                CMP     1, R11
                RBRA    _OUTPUT_DW_END, !Z

                MOVE    DEVICE_HANDLE, R8
                ADD     R10, R8
                MOVE    @R8, R8
                SYSCALL(puthex, 1)

_OUTPUT_DW_END  SYSCALL(crlf, 1)                
                MOVE    R0, R8

                DECRB
                RET

;=============================================================================
; Mount structure (variable) and string constants
;=============================================================================

DEVICE_HANDLE   .BLOCK 17                       ; mount struct / device handle

STR_TITLE       .ASCII_W "SD Card development testbed\n===========================\n\n"
STR_OK          .ASCII_W "OK"
STR_FAILED      .ASCII_W "FAILED: "
STR_SPACE1      .ASCII_W " "
STR_SPACE2      .ASCII_W "  "
STR_RESET       .ASCII_W "Resetting SD Card: "
STR_REGCHK_T    .ASCII_W "Register write and read-back:\n"
STR_REGCHK_R    .ASCII_W "    checking "
STR_REGCHK_AL   .ASCII_W "ADDR_LO: "
STR_REGCHK_AH   .ASCII_W "ADDR_HI: "
STR_REGCHK_DP   .ASCII_W "DATA_POS: "
STR_REGCHK_DTA  .ASCII_W "DATA: "
STR_REGCHK_ER   .ASCII_W "ERROR: "
STR_REGCHK_CS   .ASCII_W "CSR: "
STR_MEN_TITLE   .ASCII_P "\nChoose a test case to proceed:\n"
                .ASCII_P "    1.   Dump raw data from SD Card\n"
                .ASCII_P "    2.   Mount partition 1 (assuming valid MBR and a FAT32 file system)\n"
                .ASCII_W "    3.   Exit\n\n"
STR_IA_TITLE    .ASCII_W "Read 512 byte block from SD card and output it:\n"
STR_IA_HIGH     .ASCII_W "    Address HIGH word: "
STR_IA_LOW      .ASCII_W "    Address LOW word:  "
STR_IA_AGAIN    .ASCII_W "Enter 'y' for reading another block: "
STR_ERR_END     .ASCII_W "\nTERMINATED DUE TO FATAL ERROR: "
STR_MNT_TILE    .ASCII_W "Mounting partition #1 of SD Card as FAT32: "
STR_MNT_FSSTART .ASCII_W "    file system start address (LBA): "
STR_MNT_FATSTRT .ASCII_W "    FAT start address (LBA): "
STR_MNT_CLSSTRT .ASCII_W "    clusters start address (LBA): "
STR_MNT_SPC     .ASCII_W "    sectors per cluster: "
STR_MNT_ROOT    .ASCII_W "    root directory first cluster (LBA): "
STR_MNT_ACTIVE  .ASCII_W "    active directory first cluster (LBA): "

;=============================================================================
;=============================================================================
;
; REUSABLE FAT32 CONSTANT DEFINITIONS AND FUNCTIONS START HERE
;
;=============================================================================
;=============================================================================

; INTERNAL CONSTANTS FOR PARSING MBR and FAT32
; (not meant to be published in sysdef.asm)

FAT32$MBR_LO            .EQU    0x0000                  ; low byte of MBRs position (linear addressing)
FAT32$MBR_HI            .EQU    0x0000                  ; high byte of MBRs position (linear addressing)
FAT32$MBR_MAGIC         .EQU    0xAA55                  ; magic word at address #510 (decoded to big endian, stored as 0x55AA)
FAT32$MBR_MAGIC_ADDR    .EQU    0x01FE                  ; absolute address of magic bytes
FAT32$MBR_PARTTBL_START .EQU    0x01BE                  ; absolute start address of the partition table
FAT32$MBR_PARTTBL_RSIZE .EQU    0x0010                  ; size of each partition table record in bytes
FAT32$MBR_PT_TYPE       .EQU    0x0004                  ; partition type address (relative to the partition table)
FAT32$MBR_PT_TYPE_C1    .EQU    0x000B                  ; type flag (alternative 1) for FAT32
FAT32$MBR_PT_TYPE_C2    .EQU    0x000C                  ; type flag (alternative 2) for FAT32
FAT32$MBR_PT_FS_START   .EQU    0x0008                  ; file system start sector (relative to the partition table)

FAT32$MAGIC             .EQU    0xAA55                  ; volume id magic
FAT32$MAGIC_OFS         .EQU    0x01FE                  ; offset of volume id magic (word)
FAT32$JMP1_1            .EQU    0x00EB                  ; Sanity check: Jump instruction to boot code:
FAT32$JMP2              .EQU    0x00E9                  ; (JMP1_1 AND JMP1_2) OR JMP_2
FAT32$JMP1_OR_2_OFS     .EQU    0x0000                  
FAT32$JMP1_2            .EQU    0x0090
FAT32$JMP1_2_OFS        .EQU    0x0002
FAT32$SANITY            .EQU    0x0000                  ; (word) must be zero on FAT32
FAT32$SANITY_OFS        .EQU    0x0016
FAT32$SECTOR_SIZE       .EQU    0x0200                  ; we assume the ubiquitous 512-byte sectors
FAT32$SECTOR_SIZE_OFS   .EQU    0x000B                  ; offset of sector size in volume id (word)
FAT32$FATNUM            .EQU    0x0002                  ; number of FATs (needs to be always two) (byte)
FAT32$FATNUM_OFS        .EQU    0x0010                  ; offset of number of FATs in volume id (byte)
FAT32$SECPERCLUS_OFS    .EQU    0x000D                  ; should be 1, 2, 4, 8, 16, 32, 64, 128 (byte)
FAT32$RSSECCNT_OFS      .EQU    0x000E                  ; number of reserved sectors (word)
FAT32$SECPERFAT_OFS     .EQU    0x0024                  ; sectors per fat, depends on disk size (dword)
FAT32$ROOTCLUS_OFS      .EQU    0x002C                  ; root directory first cluster (dword)

FAT32$FE_SIZE           .EQU    0x0020                  ; size of one directory entry
FAT32$FE_DEL            .EQU    0x00E5                  ; flag for deleted (or "empty") entry
FAT32$FE_NOMORE         .EQU    0x0000                  ; flag for last deleted (or "empty") entry, no more are following
FAT32$FE_ATTRIB         .EQU    0x000B                  ; offset of the file attribute
FAT32$FE_DISPLAYCASE    .EQU    0x000C                  ; only for short names: bit 3 = display filename lower case, bit 4 = ext. lower case
FAT32$FE_DC_NAME_MASK   .EQU    0x0008                  ; FAT32$FE_DISPLAYCASE: filter for bit 3
FAT32$FE_DC_EXT_MASK    .EQU    0x0010                  ; FAT32$FE_DISPLAYCASE: filter for bit 4
FAT32$FE_SPECIAL_CHAR   .EQU    0x0005                  ; if the first char is this, then replace by E5
FAT32$FE_PADDING        .EQU    0x0020                  ; padding used in short file names

FAT32$INT_LONG_NAME     .EQU    0x000F                  ; internal flag used to filter for long file names
FAT32$INT_LONG_MASK     .EQU    0x003F                  ; internal mask for filtering long file file and directory names

FAT32$PRINT_DE_DIR_Y    .ASCII_W "<DIR> "
FAT32$PRINT_DE_DIR_N    .ASCII_W "      "
FAT32$PRINT_DE_AN       .ASCII_W " "
FAT32$PRINT_DE_AH       .ASCII_W "H"
FAT32$PRINT_DE_AR       .ASCII_W "R"
FAT32$PRINT_DE_AS       .ASCII_W "S"
FAT32$PRINT_DE_AA       .ASCII_W "A"

;
;*****************************************************************************
;* SD$RESET resets the SD Card.
;*
;* R8: 0, if everything went OK, otherwise the error code
;*****************************************************************************
;
SD$RESET        INCRB
                MOVE    IO$SD_CSR, R0                
                MOVE    SD$CMD_RESET, @R0
                RSUB    SD$WAIT_BUSY, 1
                DECRB
                RET
;
;*****************************************************************************
;* SD$READ_BLOCK reads a 512 byte block from the SD Card. 
;*
;* INPUT:  R8/R9 = LO/HI words of the 32-bit read address
;* OUTPUT: R8 = 0 (no error), or error code
;*
;* The read data is stored inside 512 byte buffer of the the SD controller 
;* memory that can then be accessed via SD$READ_BYTE.
;*
;* IMPORTANT: The 32-bit read address must be a multiple of 512, otherwise it
;* will be automatically "down rounded" to the nearest 512 byte block.
;*****************************************************************************
;
SD$READ_BLOCK   INCRB

                MOVE    R8, R1                  ; save R8 due to WAIT_BUSY

                RSUB    SD$WAIT_BUSY, 1         ; wait to be ready
                CMP     R8, 0                   ; error?
                RBRA    _SD$RB_END, !Z          ; yes: return

                MOVE    IO$SD_ADDR_LO, R0       ; lo word of 32-bit address
                MOVE    R1, @R0
                MOVE    IO$SD_ADDR_HI, R0       ; hi word of 32-bit address
                MOVE    R9, @R0
                MOVE    IO$SD_CSR, R0
                MOVE    SD$CMD_READ, @R0        ; issue block read command
                RSUB    SD$WAIT_BUSY, 1         ; wait until finished

_SD$RB_END      DECRB
                RET
;
;*****************************************************************************
;* SD$WRITE_BLOCK writes a 512 byte block to the SD Card.
;*
;* @TODO: Implement and document
;*****************************************************************************
;
SD$WRITE_BLOCK  INCRB
                DECRB
                RET
;
;*****************************************************************************
;* SD$READ_BYTE reads a byte from the read buffer memory of the controller.
;*
;* INPUT:  R8 = address between 0 .. 511
;* OUTPUT: R8 = byte
;*
;* No boundary checks are performed.
;*****************************************************************************
;
SD$READ_BYTE    INCRB

                MOVE    IO$SD_DATA_POS, R0
                MOVE    R8, @R0
                MOVE    IO$SD_DATA, R0
                MOVE    @R0, R8

                DECRB
                RET
;
;*****************************************************************************
;* SD$WRITE_BYTE writes a byte to the write memory buffer of the controller.
;*
;* @TODO: Implement and document
;*****************************************************************************
;
SD$WRITE_BYTE   INCRB
                DECRB
                RET
;
;*****************************************************************************
;* SD$WAIT_BUSY waits, while the SD Card is executing any command.
;*
;* R8: 0, if everything went OK, otherwise the error code
;*
;* Side effect: Starts the cycle counter (if it was stopped), but does not
;* reset the value, so that other countings are not influenced. 
;*****************************************************************************
;
SD$WAIT_BUSY    INCRB

                ; Make sure that the cycle counter is running for being
                ; able to measure the timeout. Do not reset it, but find
                ; the target value via addition (wrap around is OK), so that
                ; other running cycle counting processes are not disturbed
                ; by this
                MOVE    IO$CYC_STATE, R0        ; make sure, the cycle counter
                OR      CYC$RUN, @R0            ; is running
                MOVE    IO$CYC_MID, R3
                MOVE    @R3, R7
                ADD     SD$TIMEOUT_MID, R7

                ; check busy status of SD card and timeout
                MOVE    IO$SD_CSR, R0           ; SD Card Command & Status
                MOVE    IO$SD_ERROR, R2         ; SD Card Errors
_SD$WAIT_BUSY_L MOVE    @R3, R1                 ; check for timeout
                CMP     R1, R7                  ; timeout reached
                RBRA    _SD$WAIT_TO, Z          ; yes: return timeout
                MOVE    @R0, R1                 ; read CSR register       
                AND     SD$BIT_BUSY, R1         ; check busy flag
                RBRA    _SD$WAIT_BUSY_L, !Z     ; loop if busy flag is set
                MOVE    @R2, R8                 ; return error value
                RBRA    _SD$WAIT_END, 1

_SD$WAIT_TO     MOVE    SD$ERR_TIMEOUT, R8
_SD$WAIT_END    DECRB
                RET  
;
;*****************************************************************************
;* FAT32$MOUNT_SD mounts a SD card partition as a FAT32 file system
;*
;* Wrapper to simplify the use of the generic FAT32$MOUNT function. Read the
;* documentation of FAT32$MOUNT to learn more.
;* 
;* INPUT:  R8 points to a 17 word large empty structure. This structure will
;*         be filled by the mount function and it therefore becomes the device
;*         handle that you need for subsequent FAT32 function calls. For being
;*         on the safe side: Instead of hardcoding "17", use the constant
;*         FAT32$DEV_STRUCT_SIZE instead.
;*         R9 partition number to mount (1 .. 4)
;* OUTPUT: R8 points to the handle (identical to the input value of R8)
;*         R9 contains 0 if OK, otherwise the error code
;*****************************************************************************
;
FAT32$MOUNT_SD  INCRB

                ; store the function pointers to the SD card specific
                ; device handling functions within the mount structure
                ; and set the partition to 1
                MOVE    R8, R0
                ADD     FAT32$DEV_RESET, R0
                MOVE    SD$RESET, @R0
                MOVE    R8, R0
                ADD     FAT32$DEV_BLOCK_READ, R0
                MOVE    SD$READ_BLOCK, @R0
                MOVE    R8, R0
                ADD     FAT32$DEV_BLOCK_WRITE, R0
                MOVE    SD$WRITE_BLOCK, @R0
                MOVE    R8, R0
                ADD     FAT32$DEV_BYTE_READ, R0
                MOVE    SD$READ_BYTE, @R0
                MOVE    R8, R0
                ADD     FAT32$DEV_BYTE_WRITE, R0
                MOVE    SD$WRITE_BYTE, @R0
                MOVE    R8, R0
                ADD     FAT32$DEV_PARTITION, R0
                MOVE    R9, @R0

                ; call master mount function
                RSUB    FAT32$MOUNT, 1

                DECRB
                RET

;*****************************************************************************
;* FAT32$MOUNT mounts a FAT32 file system on arbitrary hardware.
;*
;* The abstraction requires 5 functions to be implemented: Read and write
;* a 512-byte-sized sector using LBA mode. Read and write a byte from within
;* a buffer that contains the current sector. Reset the device. The function
;* signatures and behaviour needs to be equivalent to the SD card functions
;* that are part of this library. You need to pass pointers to these functions
;* to the mount function call in the mount initialization structure.
;*
;* All subsequent calls to FAT32 functions expect as the first parameter a
;* pointer to the mount data structure (aka device handle) that is being
;* generated during the execution of this function. With this mechanism, an
;* arbitrary amount of filesystems can be mounted on an arbitrary amount and
;* type of hardware.
;*
;* INPUT: R8: pointer to the mount initialization structure that is build up
;* in the following form. Important: The structure is 17 words large, that
;* means that a call to FAT32$MOUNT will append more words to the structure
;* than the ones, that have to be pre-filled before calling FAT32$MOUNT:
;*  word #0: pointer to a device reset function, similar to SD$RESET
;*  word #1: pointer to a block read function, similar to SD$READ_BLOCK
;*  word #2: pointer to a block write function, similar to SD$WRITE_BLOCK
;*  word #3: pointer to a byte read function, similar to SD$READ_BYTE
;*  word #4: pointer to a byte write function, similar to SD$WRITE_BYTE
;*  word #5: number of the partition to be mounted (0x0001 .. 0x0004)
;*  word #6 .. word #17 : will be filled by by FAT32$MOUNT, their layout is
;*                        as described in the FAT32$DEV_* constants beginning
;*                        from index #7 on.
;*
;* For being on the safe side: Instead of hardcoding "17" as the size of the
;* whole mount data structure (device handle) use the constant 
;* FAT32$DEV_STRUCT_SIZE instead.
;*
;* OUTPUT: R8 is preserved and still points to the structure that has been
;* filled by the function from word #6 on.
;* R9 contains 0, if everything went OK, otherwise it contains the error code
;*****************************************************************************
;
FAT32$MOUNT     INCRB                           ; sometimes more registers ..
                INCRB                           ; .. are needed, so 2x INCRB

                MOVE    R8, R0                  ; save pointer to structure

                MOVE    R8, @--SP
                MOVE    R10, @--SP
                MOVE    R11, @--SP
                MOVE    R12, @--SP

                ; reset the device and exit on error
_F32_MNT_RESET  MOVE    FAT32$DEV_RESET, R10
                MOVE    R0, R11
                RSUB    FAT32$CALL_DEV, 1
                MOVE    R8, R9
                RBRA    _F32_MNT_END, !Z

                ; read the Master Boot record and sector and check the
                ; magic bytes as a first indication of a working MBR
                MOVE    FAT32$MBR_LO, R8            ; address (LBA) of MBR
                MOVE    FAT32$MBR_HI, R9
                MOVE    FAT32$DEV_BLOCK_READ, R10   ; read 512 byte sector
                MOVE    R0, R11
                RSUB    FAT32$CALL_DEV, 1
                CMP     0, R8
                RBRA    _F32_MNT_MBR, Z
                MOVE    R8, R9
                RBRA    _F32_MNT_END, 1
_F32_MNT_MBR    MOVE    R0, R8
                MOVE    FAT32$MBR_MAGIC_ADDR, R9    ; read magic word
                RSUB    FAT32$READ_W, 1
                CMP     FAT32$MBR_MAGIC, R10        ; compare magic word
                RBRA    _F32_MNT_PNCHK, Z           ; magic correct: go on
                MOVE    FAT32$ERR_MBR, R9           ; magic wrong: error code
                RBRA    _F32_MNT_END, 1

                ; calculate partition table start offset: this is defined as:
                ;     FAT32$MBR_PARTTBL_START +
                ;     ((#partition - 1) x FAT32$MBR_PARTTBL_RSIZE)
                ; we need to subtract 1 from #partition, because the number
                ; of partitions are defined to be between 1 and 4
_F32_MNT_PNCHK  MOVE    R0, R8                      ; get device handle
                ADD     FAT32$DEV_PARTITION, R8     ; retrieve the partition..
                MOVE    @R8, R8                     ; ..that shall be mounted
                CMP     R8, 0                       ; partition must be 1 .. 4
                RBRA    _F32_MNT_PNGOK, N           ; partition is > 0: OK
_F32_MNT_PNERR  MOVE    FAT32$ERR_PARTITION_NO, R9  ; exit with an error
                RBRA    _F32_MNT_END, 1
_F32_MNT_PNGOK  CMP     R8, 4
                RBRA    _F32_MNT_POFFS, !N          ; partition is <= 4: OK 
                RBRA    _F32_MNT_PNERR, 1           ; else exit with an error
_F32_MNT_POFFS  SUB     1, R8                       ; #partition - 1
                MOVE    FAT32$MBR_PARTTBL_RSIZE, R9 ; mult. with record size
                SYSCALL(mulu, 1)                    ; result is in R10
                ADD     FAT32$MBR_PARTTBL_START, R10
                MOVE    R10, R12                    ; offset is now in R12

                ; read and decode the selected partition table entry:
                ; 1. check for the FAT32 type description
                ; 2. check, if the linear start address of the file system
                ;    of the first partition is less than ~31MB, because
                ;    otherwise we would need better multiplication functions
                ;    (see explanation about hardcoded partition 1 above)
                ; 3. calculate the linar start address by multiplying the
                ;    sector number with 512
_F32_MNT_RPAR   MOVE    R12, R8                     ; partition table offset
                ADD     FAT32$MBR_PT_TYPE, R8       ; type flag: FAT32 ?
                MOVE    FAT32$DEV_BYTE_READ, R10
                MOVE    R0, R11
                RSUB    FAT32$CALL_DEV, 1
                CMP     FAT32$MBR_PT_TYPE_C1, R8    ; check for alternative 1
                RBRA    _F32_MNT_TCOK, Z            ; OK: continue
                CMP     FAT32$MBR_PT_TYPE_C2, R8    ; check for alternative 2
                RBRA    _F32_MNT_TCOK, Z            ; OK: continue
                MOVE    FAT32$ERR_PARTTBL, R9       ; not OK: error code
                RBRA    _F32_MNT_END, 1  
_F32_MNT_TCOK   MOVE    R0, R8                      ; device handle
                MOVE    R12, R9                     ; partition table offset
                ADD     FAT32$MBR_PT_FS_START, R9   ; find FS start sector
                RSUB    FAT32$READ_DW, 1
                MOVE    R0, R1                      ; device handle
                ADD     FAT32$DEV_FS_LO, R1         ; FS start LBA low word
                MOVE    R10, @R1                    ; store it in device hndl
                MOVE    R0, R1
                ADD     FAT32$DEV_FS_HI, R1         ; FS start LBA low word
                MOVE    R11, @R1                    ; store it in device hndl

                ; Go to the first 512 byte sector of the file system (FS)
                ; and read it. For doing so, a 2 x 32bit multiplication
                ; needs to be utilized, because the FS start LBA
                ; FAT32$DEV_FS_LO / FAT32$DEV_FS_HI is 32bit and this needs
                ; to be multiplied by FAT32$SECTOR_SIZE (512) to obtain
                ; the linar address. But the upper two words of the 64bit
                ; result value need to be zero, otherwise the medium is too
                ; large to be handled by this FAT32 implementation
                MOVE    R10, R8
                MOVE    R11, R9
                MOVE    FAT32$SECTOR_SIZE, R10
                MOVE    0, R11
                RSUB    MULU32, 1
                CMP     0, R11
                RBRA    _F32_MNT_SERR, !Z
                CMP     0, R10
                RBRA    _F32_MNT_SERR, !Z
                RBRA    _F32_MNT_DVID, 1
_F32_MNT_SERR   MOVE    FAT32$ERR_SIZE, R9
                RBRA    _F32_MNT_END, 1
               
                ; decode the FAT32 Volume ID (1st 512 byte sector)
                ; 1. sanity check: jump instruction to boot code
                ; 2. sanity check: word offs 22 must be zero on FAT32
                ; 3. check for magic
                ; 4. check for 512-byte sector size
                ; 5. check for two FATs
                ; 6. read sectors per fat and check if the hi word of sectors
                ;    per fat is zero, as we otherwise would again run into
                ;    multiplication problems (see above)
                ; 7. read root directory first cluster and check if the hi
                ;    word is zero for the same reason
                ; 8. read sectors per cluster
                ; 9. read reserved sectors
_F32_MNT_DVID   MOVE    FAT32$DEV_BLOCK_READ, R10
                MOVE    R0, R11
                RSUB    FAT32$CALL_DEV, 1
                CMP     0, R8
                RBRA    _F32_MNT_VID, Z
                MOVE    R8, R9
                RBRA    _F32_MNT_END, 1
_F32_MNT_VID    MOVE    R0, R8
                MOVE    FAT32$JMP1_OR_2_OFS, R9     ; sanity check: jump inst.
                RSUB    FAT32$READ_B, 1
                CMP     FAT32$JMP2, R10
                RBRA    _F32_MNT_SAN, Z
                CMP     FAT32$JMP1_1, R10
                RBRA    _F32_MNT_VERR, !Z
                MOVE    FAT32$JMP1_2_OFS, R9
                RSUB    FAT32$READ_B, 1
                CMP     FAT32$JMP1_2, R10
                RBRA    _F32_MNT_VERR, !Z
_F32_MNT_SAN    MOVE    FAT32$SANITY_OFS, R9
                RSUB    FAT32$READ_W, 1
                CMP     FAT32$SANITY, R10
                RBRA    _F32_MNT_MGC, Z
                RBRA    _F32_MNT_VERR, 1                
_F32_MNT_MGC    MOVE    FAT32$MAGIC_OFS, R9         ; check magic
                RSUB    FAT32$READ_W, 1
                CMP     FAT32$MAGIC, R10
                RBRA    _F32_MNT_VERR, !Z
                MOVE    FAT32$SECTOR_SIZE_OFS, R9   ; check 512 byte sector
                RSUB    FAT32$READ_W, 1
                CMP     FAT32$SECTOR_SIZE, R10
                RBRA    _F32_MNT_VERR, !Z
                MOVE    FAT32$FATNUM_OFS, R9        ; check for two FATs
                RSUB    FAT32$READ_B, 1
                CMP     FAT32$FATNUM, R10
                RBRA    _F32_MNT_VERR, !Z
                RBRA    _F32_MNT_RVID, 1
_F32_MNT_VERR   MOVE    FAT32$ERR_NOFAT32, R9
                RBRA    _F32_MNT_END, 1
_F32_MNT_RVID   MOVE    R0, R8
                MOVE    FAT32$SECPERFAT_OFS, R9     ; read sectors per FAT
                RSUB    FAT32$READ_DW, 1
                MOVE    R10, R1                     ; R1: sectors per FAT LO
                MOVE    R11, R2                     ; R2: sectors per FAT HI
                MOVE    FAT32$ROOTCLUS_OFS, R9      ; read root dir 1st clus.
                RSUB    FAT32$READ_DW, 1
                MOVE    R10, R3                     ; R3: rootdir 1st clus. LO
                MOVE    R11, R4                     ; R4: rootdir 1st clus. HI
                MOVE    FAT32$SECPERCLUS_OFS, R9    ; read sectors per cluster
                RSUB    FAT32$READ_B, 1
                MOVE    R10, R5                     ; R5: sectors per cluster
                MOVE    FAT32$RSSECCNT_OFS, R9      ; read number resvd. sec.
                RSUB    FAT32$READ_W, 1
                MOVE    R10, R6                     ; R6: # reserved sectors

                ; calculate begin of FAT (LBA)
                ; fat_begin_lba = Partition_LBA_Begin + Num_of_Rsvd_Sectors
                MOVE    R0, R8
                ADD     FAT32$DEV_FS_LO, R8
                MOVE    @R8, R8                     ; FS start: LO
                MOVE    R0, R9
                ADD     FAT32$DEV_FS_HI, R9
                MOVE    @R9, R9                     ; FS start: HI
                ADD     R6, R8                      ; 32bit add resvd. sect.
                ADDC    0, R9
                MOVE    R0, R7
                ADD     FAT32$DEV_FAT_LO, R7        ; store it in device hndl
                MOVE    R8, @R7
                MOVE    R0, R7
                ADD     FAT32$DEV_FAT_HI, R7
                MOVE    R9, @R7

                ; calculate begin of clusters (LBA)
                ; clust_begin_lba = fat_begin_lba + (Num_FATs * Sect_per_FAT)
                DECRB
                MOVE    R8, R0                      ; save FAT start LO
                MOVE    R9, R1                      ; save FAT start HI
                INCRB
                MOVE    2, R8                       ; Num_FATs is hardcoded 2
                MOVE    0, R9
                MOVE    R1, R10                     ; sectors per fat LO
                MOVE    R2, R11                     ; sectors per fat HI
                RSUB    MULU32, 1
                CMP     0, R10
                RBRA    _F32_MNT_SERR, !Z
                CMP     0, R11
                RBRA    _F32_MNT_SERR, !Z
                DECRB
                MOVE    R0, R10                     ; restore FAT start LO
                MOVE    R1, R11                     ; restore FAT start HI
                INCRB
                ADD     R10, R8                     ; 32bit addition ..
                ADDC    R11, R9                     ; .. result is in R9|R8
                MOVE    R0, R10
                ADD     FAT32$DEV_CLUSTER_LO, R10
                MOVE    R8, @R10                    ; store result LO
                MOVE    R0, R10
                ADD     FAT32$DEV_CLUSTER_HI, R10
                MOVE    R9, @R10                    ; store result HI

                ; store sectors per cluster and root directory 1st cluster
                ; and set currently active directory to root directory
                MOVE    R0, R10
                ADD     FAT32$DEV_SECT_PER_CLUS, R10
                MOVE    R5, @R10
                MOVE    R0, R10
                ADD     FAT32$DEV_RD_1STCLUS_LO, R10
                MOVE    R3, @R10
                MOVE    R0, R10
                ADD     FAT32$DEV_RD_1STCLUS_HI, R10
                MOVE    R4, @R10
                MOVE    R0, R10
                ADD     FAT32$DEV_AD_1STCLUS_LO, R10
                MOVE    R3, @R10
                MOVE    R0, R10
                ADD     FAT32$DEV_AD_1STCLUS_HI, R10
                MOVE    R4, @R10

                MOVE    0, R9                       ; no errors occured

_F32_MNT_END    MOVE    @SP++, R12
                MOVE    @SP++, R11                  ; restore registers
                MOVE    @SP++, R10
                MOVE    @SP++, R8

                DECRB
                DECRB
                RET
;
;*****************************************************************************
;* FAT32$DIR_OPEN opens the current directory for browsing
;*
;* Call this function, before working with FAT32$DIR_LIST. Directly after
;* mounting a device, the "current directory" equals the root directory.
;*
;* INPUT:  R8  points to a valid device handle
;*         R9  points to an empty directory handle struct that will be filled
;*             (use FAT32$FDH_STRUCT_SIZE to reserve the memory)
;* OUTPUT: R8  points to the filled directory handle structure, i.e. it points
;*             to where R9 originally pointed to
;*         R9  0, if OK, otherwise error code
;*****************************************************************************
;
FAT32$DIR_OPEN  INCRB

                MOVE    R9, R0                      ; save device handle
                ADD     FAT32$FDH_DEVICE, R0
                MOVE    R8, @R0

                MOVE    R8, R0                      ; save the cluster of ...
                ADD     FAT32$DEV_AD_1STCLUS_LO, R0 ; ... the current dir.                
                MOVE    R9, R1
                ADD     FAT32$FDH_CLUSTER_LO, R1
                MOVE    @R0, @R1
                MOVE    @R0, R2
                MOVE    R8, R0
                ADD     FAT32$DEV_AD_1STCLUS_HI, R0
                MOVE    R9, R1
                ADD     FAT32$FDH_CLUSTER_HI, R1
                MOVE    @R0, @R1
                MOVE    @R0, R3

                MOVE    R9, R0                      ; start with sector 0
                ADD     FAT32$FDH_SECTOR, R0
                MOVE    0, @R0

                MOVE    R9, R0                      ; start with index 0
                ADD     FAT32$FDH_INDEX, R0
                MOVE    0, @R0 

                MOVE    R9, R0                      ; remember dir. handle

                MOVE    R2, R9                      ; low word of cluster
                MOVE    R3, R10                     ; high word of cluster
                MOVE    0, R11                      ; sector
                RSUB    FAT32$READ_SIC, 1           ; R9 contains OK or error

                MOVE    R0, R8                      ; return dir. handle

                DECRB
                RET
;
;*****************************************************************************
;* FAT32$DIR_LIST is used to browse the currently active directory.
;*
;* This function is meant to be called iteratively to return one directory
;* entry after the other. It supports long filenames. The implementation
;* supports a maximum of 65.535 files within one folder.
;*
;* INPUT:  R8  points to a valid directory handle (created by FAT32$DIR_OPEN)
;*         R9  points to am empty directoy entry structure, having the size of
;*             FAT32$DE_STRUCT_SIZE, that will be filled by this function.
;*         R10 or-ed list of FAT32$FA_* flags to filter for certain types:
;*             if the attribute is not set, then an entry having this flag
;*             will not be browsed (i.e. it will be hidden). Use
;*             FAT32$FA_DEFAULT, if you want to browse for all non hidden
;*             files and directories.
;* OUTPUT: R8  still points to the directory handle
;*         R9  points to the now filled directory entry structure
;*         R10 1, if the current entry is a valid entry and therefore another
;*             iteration (i.e. call to FAT32$DIR_LIST) makes sense
;*             0, if the current entry is not valid and therefore the end
;*             of the directory has been reached (R11 is 0 in such a case)
;*         R11 0, if OK, otherwise error code
;*****************************************************************************
;
FAT32$DIR_LIST  INCRB                               ; if referenced in
                MOVE    R8, R0                      ; comments, the registers
                MOVE    R9, R1                      ; of this bank are 
                MOVE    R12, R7                     ; prefixed with a §

                MOVE    R10, R2                     ; §R2 = attrib filters
                                                    ; §R3 and §R4 used below
                INCRB

                MOVE    R8, R0
                ADD     FAT32$FDH_DEVICE, R0
                MOVE    @R0, R0                     ; R0 = device handle
                MOVE    R8, R1                      ; R1 = directory handle
                MOVE    R9, R2                      ; R2 = dir. entry struct.

                ; make sure that the 512-byte read-buffer contains the
                ; sector that is needed, and in parallel update the file and
                ; directory handle, if necessary
_F32_DLST_SKIP  RSUB    FAT32$READ_FDH, 1
                CMP     R9, 0
                RBRA    _F32_DLST_C1, Z
                MOVE    0, R10
                MOVE    R9, R11                
                RBRA    _F32_DLST_END, 1

                ; read the first byte (byte #0)
_F32_DLST_C1    MOVE    R0, R8                      ; R8 = device handle
                MOVE    R1, R3 
                ADD     FAT32$FDH_INDEX, R3         ; R3 = pointer to index
                MOVE    @R3, R4                     ; R4 = index aka byte addr
                MOVE    R4,  R9                     ; R9 = address of byte
                RSUB    FAT32$READ_B, 1             ; read byte to R10

                ; check if we reached the end of the directory
                CMP     R10, FAT32$FE_NOMORE        ; last entry?
                RBRA    _F32_DLST_C2, !Z            ; no: go on checking
                MOVE    0, R10                      ; yes, EOD reached
                MOVE    0, R11
                RBRA    _F32_DLST_END, 1

                ; check, if the entry marks a deleted entry and if yes
                ; skip it
_F32_DLST_C2    CMP     R10, FAT32$FE_DEL           ; deleted entry?
                RBRA    _F32_DLST_C3, !Z            ; no: go on checking
_F32_DLST_DS    ADD     FAT32$FE_SIZE, R4           ; set index to next record
                MOVE    R4, @R3                     ; write back idx to FDH
                MOVE    R1, R8                      ; R8 = directory handle
                RBRA    _F32_DLST_SKIP, 1           ; skip entry, read next

                ; special first character handling
_F32_DLST_C3    CMP     R10, FAT32$FE_SPECIAL_CHAR  ; special char?
                RBRA    _F32_DLST_C4, !Z            ; no: go on
                MOVE    FAT32$FE_DEL, R10           ; yes: set the special chr

                ; store first read character to dir. entry struct.
_F32_DLST_C4    MOVE    R2, R5                      ; dir. entry struct.
                ADD     FAT32$DE_NAME, R5           ; R5 = offs. to name
                MOVE    R10, @R5                    ; store character #0                

                ; retrieve attributes and store them to the dir. entry struct.
                MOVE    R0, R8                      ; R8 = device handle
                ADD     FAT32$FE_ATTRIB, R9         ; offset for entry attrib.
                RSUB    FAT32$READ_B, 1             ; read attribute to R10
                MOVE    R2, R12                     ; store attribute
                ADD     FAT32$DE_ATTRIB, R12
                MOVE    R10, @R12

                ; long name?
                MOVE    R10, R7
                AND     FAT32$INT_LONG_MASK, R7
                CMP     R7, FAT32$INT_LONG_NAME
                RBRA    _F32_DLST_SN1, !Z           ; no: short name
                RBRA    _F32_DLST_DS, 1             ; yes: skip entry

                ; short name: apply attribute filter
_F32_DLST_SN1   DECRB
                MOVE    R2, R8                      ; §R2 = attrib filter
                INCRB
                NOT     R8, R8                      ; the attribs not set..
                AND     R8, R10                     ; ..shall be filtered out
                RBRA    _F32_DLST_DS, !Z            ; so skip if != 0

                ; short name: find out if the file name and/or the extension
                ; shall be displayed as lower case characters
                MOVE    R0, R8                      ; R8 = device handle
                MOVE    R4, R9                      ; R9 = index to be read
                ADD     FAT32$FE_DISPLAYCASE, R9
                RSUB    FAT32$READ_B, 1             ; R10 = read flags
                DECRB                               ; store result in other
                                                    ; register bank
                MOVE    0, R3                       ; §R3 = lower case name?
                MOVE    0, R4                       ; §R4 = lower case ext?
                MOVE    R10, R9                     ; lower case name?
                AND     FAT32$FE_DC_NAME_MASK, R9
                RBRA    _F32_DLST_SN2, Z
                MOVE    1, R3                       ; yes: §R3 set to "true"
                INCRB
                MOVE    R8, R12                     ; lower case 1st character
                MOVE    @R5, R8
                RSUB    TO_LOWER, 1
                MOVE    R8, @R5
                MOVE    R12, R8
                DECRB
_F32_DLST_SN2   MOVE    R10, R9
                AND     FAT32$FE_DC_EXT_MASK, R9    ; lower case extension?
                RBRA    _F32_DLST_SN3, Z
                MOVE    1, R4                       ; yes: §R4 set to "true"
_F32_DLST_SN3   INCRB

                ; short name: retrieve it from index #1 (second character)
                ; because the char from index #0 has already been retrieved;
                ; the 8.3 filenames are stored as 11 bytes using 0x20 to
                ; fill/pad and the "." is not stored; as we already did
                ; read 1 byte, we still need to read 10 more bytes
                ADD     1, R5                       ; R5 points to name
                MOVE    2, R6                       ; char num (cnt from 1)
                MOVE    R4, R9                      ; R9 = index to char #0
_F32_DLST_SBL1  ADD     1, R9                       ; next character
                RSUB    FAT32$READ_B, 1             ; read byte to R10
                CMP     R6, 8                       ; still in the name part?                
                RBRA    _F32_DLST_SBL2, N           ; no: extension part
                DECRB
                MOVE    R3, R12                     ; §R3 true? low. cs. name
                INCRB
                CMP     R12, 1                      ; lower case name?
                RBRA    _F32_FLST_SBL3, !Z          ; no: go on
                MOVE    R8, R12                     ; yes: convert to lower
                MOVE    R10, R8
                RSUB    TO_LOWER, 1
                MOVE    R8, R10
                MOVE    R12, R8
                RBRA    _F32_FLST_SBL3, 1 
_F32_DLST_SBL2  DECRB
                MOVE    R4, R12                     ; §R4 true? low. cs. ext
                INCRB
                CMP     R12, 1                      ; lower case extension?
                RBRA    _F32_FLST_SBL3, !Z          ; no: go on
                MOVE    R8, R12                     ; yes: convert to lower
                MOVE    R10, R8
                RSUB    TO_LOWER, 1
                MOVE    R8, R10
                MOVE    R12, R8
_F32_FLST_SBL3  CMP     R10, FAT32$FE_PADDING       ; padding characters?
                RBRA    _F32_FLST_SBL4, Z           ; yes: ignore it
                MOVE    R10, @R5++                  ; no: store character
_F32_FLST_SBL4  CMP     R6, 8                       ; add a "." after 8th chr
                RBRA    _F32_FLST_SBL5, !Z          ; not the 8th character
                MOVE    '.', @R5++
_F32_FLST_SBL5  ADD     1, R6                       ; one more char is read
                CMP     R6, 11                      ; all chars read?
                RBRA    _F32_DLST_SBL1, !N          ; one more to go?
                MOVE    0, @R5                      ; add zero terminator

                ; update index to next directory entry and return
_F32_DLST_NI    ADD     FAT32$FE_SIZE, R4           ; update index
                MOVE    R4, @R3                     ; store it to FDH
                MOVE    1, R10                      ; return "valid entry"
                MOVE    0, R11                      ; return "no errors"

_F32_DLST_END   DECRB                               ; restore R8, R9, R12
                MOVE    R0, R8
                MOVE    R1, R9
                MOVE    R7, R12
                DECRB
                RET
;
;*****************************************************************************
;* FAT32$CALL_DEV calls a device management function
;*
;* INPUT:  R8, R9 are the parameters to the function
;*         R10 is the function index
;*         R11 is the mount data structure (device handle)
;*
;* OUTPUT: R8 is the return value from the function
;*****************************************************************************
;
FAT32$CALL_DEV  INCRB

                MOVE    R11, R0                 ; compute function address
                ADD     R10, R0
                MOVE    _F32$CD_END, @--SP      ; compute return address

                MOVE    @R0, PC                 ; perform function call

_F32$CD_END     DECRB
                RET
;
;*****************************************************************************
;* FAT32$READ_FDH fills the read buffer according to the current index in FDH
;*
;* If the index within FDH is < FAT32$SECTOR_SIZE (512), then it is assumed,
;* that no read operation needs to be performed, i.e. another function has
;* already filled the 512-byte read-buffer. Otherwise, the sector is
;* increased (and the index is reset to 0) and if necessary also the cluster
;* is increased (and the sector is reset to 0). The new index, sector and
;* cluster values are stored within the FDH (file and directory handle).
;* In case of an increased index or sector or cluster value, the 512-byte
;* read-buffer is re-read for subsequent read accesses.
;*
;* INPUT:  R8: FDH
;* OUTPUT: R8: FDH
;*         R9: 0, if OK, otherwise error code
;*****************************************************************************
;
FAT32$READ_FDH  INCRB
                MOVE    R8, R0
                MOVE    R10, R1
                MOVE    R11, R2
                INCRB

                MOVE    R8, R0
                ADD     FAT32$FDH_DEVICE, R0
                MOVE    @R0, R0                     ; R0 = device handle
                MOVE    R8, R1                      ; R1 = directory handle

                ; if the current "to-be-read" index equals 512, then
                ; we need to read the next sector within the cluster
                MOVE    R1, R2
                ADD     FAT32$FDH_INDEX, R2
                MOVE    @R2, R3                     ; R3 = "to-be-read" index
                CMP     R3, FAT32$SECTOR_SIZE
                RBRA    _F32_RFDH_CISS, !Z

                ; reset the index and increase the sector
                ; if the sector is larger than the sectors per cluster, then
                ; we need to increase the cluster (i.e. look up the next one
                ; in the FAT); otherwise we can just read the new sector
                ; within the same cluster
                MOVE    0, @R2                      ; write back resetted idx
                MOVE    0, R3                       ; R3 = "to-be-read" index
                MOVE    R1, R2
                ADD     FAT32$FDH_SECTOR, R2
                MOVE    @R2, R4
                ADD     1, R4                       ; R4 = increased sector
                MOVE    R0, R2
                ADD     FAT32$DEV_SECT_PER_CLUS, R2
                MOVE    @R2, R2
                SUB     1, R2                       ; we count from 0
                CMP     R4, R2                      ; R4 > sectors per clus.?
                RBRA    _F32_RFDH_INCC, N           ; yes: next cluster
                MOVE    R1, R2                      ; no: write back ...
                ADD     FAT32$FDH_SECTOR, R2        ; ... increased sector
                MOVE    R4, @R2
                MOVE    R0, R8
                MOVE    R1, R2
                ADD     FAT32$FDH_CLUSTER_LO, R2
                MOVE    @R2, R9                     ; LO word of cluster
                MOVE    R1, R2
                ADD     FAT32$FDH_CLUSTER_HI, R2
                MOVE    @R2, R10                    ; HI word of cluster
                MOVE    R4, R11                     ; sector number
                RSUB    FAT32$READ_SIC, 1           ; read sector in cluster
                RBRA    _F32_RFDH_END, 1

                ; next cluster
_F32_RFDH_INCC  MOVE    R1, R2                      
                ADD     FAT32$FDH_SECTOR, R2
                MOVE    0, @R2                      ; write back sector = 0
                HALT ; @TODO: implement
                RBRA    _F32_RFDH_DONE, 1


                ; check for access beyond the sector size (means illegal hndl)
_F32_RFDH_CISS  CMP     R3, FAT32$SECTOR_SIZE
                RBRA    _F32_RFDH_DONE, !N
                MOVE    FAT32$ERR_CORRUPT_DH, R9
                RBRA    _F32_RFDH_END, 1

_F32_RFDH_DONE  MOVE    0, R9
_F32_RFDH_END   DECRB
                MOVE    R0, R8
                MOVE    R1, R10
                MOVE    R2, R11
                DECRB
                RET
;
;*****************************************************************************
;* FAT32$READ_SIC reads a sector within a cluster
;*
;* INPUT:   R8:  device handle
;*          R9:  LO word of cluster
;*          R10: HI word of cluster
;*          R11: sector within cluster
;* OUTPUT:  R8:  device handle
;           R9:  0, if OK, otherweise error code
;           R10 and R11 are destroyed
;*****************************************************************************
;
FAT32$READ_SIC  INCRB

                ; if sector within cluster is larger than the amount of
                ; clusters per sector then exit with an error message
                MOVE    R8, R0
                ADD     FAT32$DEV_SECT_PER_CLUS, R0
                MOVE    @R0, R0
                SUB     1, R0                       ; we start counting from 0
                CMP     R11, R0
                RBRA    _F32_RSIC_C1, !N
                MOVE    FAT32$ERR_ILLEGAL_SIC, R9
                RBRA    _F32_RSIC_END, 1

                ; all clusters numbers need to be >= 2
_F32_RSIC_C1    CMP     R10, 0                      ; if hi word != 0 then ...
                RBRA    _F32_RSIC_C2, !Z            ; it is for sure >= 2
                CMP     R9, 1                       ; if low word > 1, then
                RBRA    _F32_RSIC_C2, N             ; it is also >= 2
                MOVE    FAT32$ERR_ILLEGAL_CLUS, R9
                RBRA    _F32_RSIC_END, 1

                ; lba_addr = cluster_begin_lba +
                ;           (cluster_number - 2) * sectors_per_cluster +
                ;           sector
_F32_RSIC_C2    MOVE    R8, R0                      ; save device handle
                MOVE    R11, R1                     ; save sector
                MOVE    R9, R8                      ; R8 = cluster LO
                MOVE    R10, R9                     ; R9 = cluster HI
                SUB     2, R8                       ; cluster = cluster - 2
                SUBC    0, R9
                MOVE    R0, R10                     ; get sectors_per_cluster
                ADD     FAT32$DEV_SECT_PER_CLUS, R10
                MOVE    @R10, R10
                MOVE    0, R11
                RSUB    MULU32, 1                   ; above mentioned "*"
                MOVE    R0, R2                      ; add cluster_begin_lba
                ADD     FAT32$DEV_CLUSTER_LO, R2
                MOVE    @R2, R2
                ADD     R2, R8
                ADDC    0, R9
                ADDC    0, R10
                ADDC    0, R11
                MOVE    R0, R2
                ADD     FAT32$DEV_CLUSTER_HI, R2
                MOVE    @R2, R2
                ADD     R2, R9
                ADDC    0, R10
                ADDC    0, R11
                ADD     R1, R8                      ; add sector
                ADDC    0, R9
                ADDC    0, R10
                ADDC    0, R11
                CMP     0, R11                      ; too large?
                RBRA    _F32_RSIC_C3, Z
                CMP     0, R10
                RBRA    _F32_RSIC_C3, Z
                MOVE    FAT32$ERR_SIZE, R9
                RBRA    _F32_RSIC_END, 1

                ; linear address = lba_addr * 512
_F32_RSIC_C3    MOVE    FAT32$SECTOR_SIZE, R10
                MOVE    0, R11
                RSUB    MULU32, 1
                CMP     0, R11                      ; too large?
                RBRA    _F32_RSIC_C4, Z
                CMP     0, R10
                RBRA    _F32_RSIC_C4, Z
                MOVE    FAT32$ERR_SIZE, R9
                RBRA    _F32_RSIC_END, 1

                ; read sector into internal buffer
_F32_RSIC_C4    MOVE    FAT32$DEV_BLOCK_READ, R10
                MOVE    R0, R11
                RSUB    FAT32$CALL_DEV, 1
                MOVE    R8, R9
                MOVE    R0, R8

_F32_RSIC_END   DECRB
                RET
;
;*****************************************************************************
;* FAT32$PRINT_DE is a pretty printer for directory entries
;*
;* Uses monitor (system) stdout to print. Allows the configuration of the
;* amount of data that shall be printed: Filename only is the minimum. 
;* Additionally attributes, file sizes, file date and file time can be shown.
;* The printed layout is as follows:
;*
;* <DIR> HRSA BBBBBBBBB YYYY-MM-DD HH:MM   name...
;*
;* <DIR> means that the entry is a directory, otherwise whitespace
;* H = hidden flag
;* R = read only flag
;* S = system flag
;* A = archive flag
;* BBBBBBBBB = decimal size of the file in bytes
;* YYYY-MM-DD = file date
;* HH:MM = file time
;* name... = file name in long file format
;*
;* INPUT:  R8:  pointer to directy entry structure
;*         R9:  print flags as defined in FAT32$PRINT_SHOW_*
;*****************************************************************************
;
FAT32$PRINT_DE  INCRB

                MOVE    R8, R0                      ; R0 = ptr to dir. ent. s.
                MOVE    R9, R1                      ; R1 = print flags

                ; print <DIR> indicator
                MOVE    R1, R2                      ; show <dir> indicator?
                AND     FAT32$PRINT_SHOW_DIR, R2
                RBRA    _F32_PDE_A1, Z              ; no: go on
                MOVE    R0, R2                      ; is current entry a dir.?
                ADD     FAT32$DE_ATTRIB, R2
                MOVE    @R2, R2
                MOVE    FAT32$PRINT_DE_DIR_N, R8    ; assume no
                AND     FAT32$FA_DIR, R2
                RBRA    _F32_PDE_D1, Z
                MOVE    FAT32$PRINT_DE_DIR_Y, R8    ; yes, it is
_F32_PDE_D1     SYSCALL(puts, 1)                    ; print <DIR> or whitespc

                ; print attributes
_F32_PDE_A1     MOVE    R1, R2                      ; show attributes?
                AND     FAT32$PRINT_SHOW_ATTRIB, R2
                RBRA    _F32_PDE_S1, Z              ; no: go on
                MOVE    R0, R2
                ADD     FAT32$DE_ATTRIB, R2
                MOVE    @R2, R3                     ; @R2 contains attrib
                MOVE    FAT32$PRINT_DE_AN, R8
                AND     FAT32$FA_ARCHIVE, R3        ; attrib = archive?
                RBRA    _F32_PDE_A2, Z
                MOVE    FAT32$PRINT_DE_AA, R8
_F32_PDE_A2     SYSCALL(puts, 1)                
                MOVE    @R2, R3                     ; @R2 contains attrib
                MOVE    FAT32$PRINT_DE_AN, R8
                AND     FAT32$FA_HIDDEN, R3         ; attrib = hidden?
                RBRA    _F32_PDE_A3, Z
                MOVE    FAT32$PRINT_DE_AH, R8
_F32_PDE_A3     SYSCALL(puts, 1)
                MOVE    @R2, R3                     ; @R2 contains attrib
                MOVE    FAT32$PRINT_DE_AN, R8
                AND     FAT32$FA_READ_ONLY, R3      ; attrib = read only?
                RBRA    _F32_PDE_A4, Z
                MOVE    FAT32$PRINT_DE_AR, R8
_F32_PDE_A4     SYSCALL(puts, 1)
                MOVE    @R2, R3                     ; @R2 contains attrib
                MOVE    FAT32$PRINT_DE_AN, R8
                AND     FAT32$FA_SYSTEM, R3         ; attrib = system?
                RBRA    _F32_PDE_A5, Z
                MOVE    FAT32$PRINT_DE_AS, R8
_F32_PDE_A5     SYSCALL(puts, 1)
                MOVE    FAT32$PRINT_DE_AN, R8
                SYSCALL(puts, 1)

                ; print size
_F32_PDE_S1     NOP

                ; print name
_F32_PDE_N1     MOVE    R0, R8
                ADD     FAT32$DE_NAME, R8
                SYSCALL(puts, 1)                    ; print name
                SYSCALL(crlf, 1)                    ; next line out stdout
                
_F32_PDE_END    MOVE    R0, R8
                MOVE    R1, R9
                DECRB
                RET                
;
;*****************************************************************************
;* FAT32$READ_B reads a byte from the current sector buffer
;*
;* INPUT:  R8:  pointer to mount data structure (device handle)
;*         R9:  address (0 .. 511)
;* OUTPUT: R10: the byte that was read
;*****************************************************************************
;
FAT32$READ_B    INCRB

                MOVE    R8, R0

                MOVE    R8, R11                 ; mount data structure
                MOVE    R9, R8                  ; read address
                MOVE    FAT32$DEV_BYTE_READ, R10
                RSUB    FAT32$CALL_DEV, 1
                MOVE    R8, R10

                MOVE    R0, R8

                DECRB
                RET
;                
;*****************************************************************************
;* FAT32$READ_W reads a word from the current sector buffer
;*
;* Assumes that the buffer is stored in little endian (as this is the case
;* for MBR and FAT32 data structures)
;* 
;* INPUT:  R8:  pointer to mount data structure (device handle)
;*         R9:  address (0 .. 511)
;* OUTPUT: R10: the word that was read
;*****************************************************************************
;
FAT32$READ_W    INCRB

                MOVE    R8, R0
                MOVE    R9, R1

                RSUB    FAT32$READ_B, 1         ; read low byte ...
                MOVE    R10, R2                 ; ... and remember it
                ADD     1, R9                   ; read high byte ...
                RSUB    FAT32$READ_B, 1
                MOVE    R10, R3                 ; ... and remember it

                SWAP    R3, R10                 ; R3 lo = high byte of R10
                OR      R2, R10                 ; R2 lo = low byte of R10

                MOVE    R0, R8
                MOVE    R1, R9

                DECRB
                RET
;
;*****************************************************************************
;* FAT32$READ_DW reads a double word from the current sector buffer
;*
;* Assumes that the buffer is stored in little endian (as this is the case
;* for MBR and FAT32 data structures)
;*
;* INPUT:  R8:  pointer to mount data structure (device handle)
;*         R9:  address (0 .. 511)
;* OUTPUT: R10: the low word that was read
;*         R11: the high word that was read
;*****************************************************************************
;
FAT32$READ_DW   INCRB
                MOVE    R9, R0

                RSUB    FAT32$READ_W, 1
                MOVE    R10, R1
                ADD     2, R9
                RSUB    FAT32$READ_W, 1
                MOVE    R10, R11
                MOVE    R1, R10

                MOVE    R0, R9
                DECRB
                RET

;=============================================================================
;=============================================================================
;
; POTENTIAL EXTENSIONS OF OTHER MONITOR LIBRARIES START HERE
;
;=============================================================================
;=============================================================================

;
;*****************************************************************************
;* MULU32 multiplies two 32bit unsigned values and returns a 64bit unsigned
;*
;* INPUT:  R8/R9   = LO/HI of unsigned multiplicant 1
;*         R10/R11 = LO/HI of unsigned multiplicant 2
;* OUTPUT: R11/R10/R9/R8 = HI .. LO of 64bit result
;*****************************************************************************
;
MULU32          INCRB                           ; registers R3..R0 = result ..
                INCRB                           ; .. therefore two INCRBs

                ; save arguments as in R1|R0 * R3|R2
                MOVE    R8, R0
                MOVE    R9, R1
                MOVE    R10, R2
                MOVE    R11, R3

                ; algorithm:
                ;       R1R0
                ; x     R3R2
                ; ----------
                ;       R2R0
                ; +   R2R1
                ; +   R3R0
                ; + R3R1
                ; ----------

                MOVE    R0, R8                  ; R2 * R0
                MOVE    R2, R9
                SYSCALL(mulu, 1)                ; result in R11|R10
                DECRB
                MOVE    R10, R0
                MOVE    R11, R1
                XOR     R2, R2
                XOR     R3, R3
                INCRB

                MOVE    R1, R8                  ; R2 * R1
                MOVE    R2, R9
                SYSCALL(mulu, 1)
                DECRB
                ADD     R10, R1
                ADDC    R11, R2
                ADDC    0, R3
                INCRB

                MOVE    R0, R8                  ; R3 * R0
                MOVE    R3, R9
                SYSCALL(mulu, 1)
                DECRB
                ADD     R10, R1
                ADDC    R11, R2
                ADDC    0, R3
                INCRB

                MOVE    R1, R8                  ; R3 * R1
                MOVE    R3, R9
                SYSCALL(mulu, 1)
                DECRB
                ADD     R10, R2
                ADDC    R11, R3

                MOVE    R3, R11                 ; store result (return values)
                MOVE    R2, R10
                MOVE    R1, R9
                MOVE    R0, R8

                DECRB
                RET
;
;*****************************************************************************
;* TO_LOWER converts the character in R8 to lower case
;* (already added to the monitor, but not synthesized yet as of 16/7/3)
;* (so this is TEMP; replace by SYSCALL, soon)
;*****************************************************************************
;
TO_LOWER        INCRB
                CMP     R8, '@'                 ; Is it "@" or less than that?
                RBRA    _TO_LOWER_EXIT, !N      ; Yes: nothing to do
                CMP     R8, 'Z'                 ; Is it greater than 'Z'
                RBRA    _TO_LOWER_EXIT, N       ; Yes: nothing to do
                ADD     0x0020, R8              ; Perform the conversion
_TO_LOWER_EXIT  DECRB
                RET