; Disassembly of a Microsoft FAT32 volume boot record, by Toby Jones.
; Disassembled around 16 March 2001.

; This code is valid for DOS/Windows boot sectors, though it is likely that modern
; boot sectors simply display an error message without attempting to load IO.SYS.

; An interesting thing about this boot sector is that it uses 286/386 instructions.
; Given that FAT32 is generally used for flash disks and other larger media,
; it is relatively safe to assume that such disks will never be used on a 8088.

; With regards to the code quality, there are numerous oddities.
;  Interrupts are disabled through the entire code.
;  The string table is unnecessarily complex, using indirection instead of direct pointers.
;  The "functions" assume to be called from specific points, with specific stack
;    unwinding for those call points.  In general, the stack usage is unstructured.
;  BP points to the start of the boot sector, instead of the start of the BPB,
;    which makes the structure offset calculations a bit more complex than necessary.
;    Yet, the code is in an absolute memory location, so an offset from BP isn't
;    even required.  Sometimes an absolute reference is made instead, further
;    distorting things.
;  The use of jumps to do carry addition, instead of using adc.
;  In general, the code locality is poor, demonstrated by the various string
;  printing functions not in the same place.

; Disk Parameter Table
struc dpt
    .HeadUnloadTime     resb 1
    .HeadLoadTime       resb 1
    .MotorDelayOff      resb 1
    .BytesPerSector     resb 1
    .SectorsPerTrack    resb 1
    .SectorGapLength    resb 1
    .DataLength         resb 1
    .FormatGapLength    resb 1
    .FormatByteValue    resb 1
    .HeadSettlingTime   resb 1
    .MotorDelayOn       resb 1
endstruc

struc partition_entry
    .Bootable       resb 1
    .BeginHead      resb 1
    .BeginSector    resb 1
    .BeginCylinder  resb 1
    .FileSystemType resb 1
    .EndHead        resb 1
    .EndSector      resb 1
    .EndCylinder    resb 1
    .StartSector    resd 1
    .Sectors        resd 1
endstruc

;----------------------------------------------------------------------------

org 7c00h

start:

    jmp     short after_data
    nop

; BIOS Parameter Block ------------------------------------------------------

%define MEDIA_DESCRIPTOR_FIXED_DISK 0f8h

struc bpb_fat32
    .OEMName            resb 8
    .BytesPerSector     resw 1
    .SectorsPerCluster  resb 1
    .ReservedSectors    resw 1
    .FATs               resb 1
    .RootEntries        resw 1  ; 0 for FAT32.
    .Sectors            resw 1  ; 0 for FAT32.
    .MediaDescriptor    resb 1
    .FATSectors         resw 1  ; 0 for FAT32.
    .SectorsPerTrack    resw 1
    .Heads              resw 1
    .HiddenSectors      resd 1
    .HugeSectors        resd 1
    .SectorsPerFAT      resd 1
    .ExtFlags           resw 1
    .FSVersion          resw 1
    .RootCluster        resd 1
    .FSInfoSector       resw 1
    .BackupSector       resw 1
    .Reserved1          resb 12
    .DriveNumber        resb 1
    .Reserved2          resb 1
    .BootSignature      resb 1
    .VolumeID           resd 1
    .VolumeLabel        resb 11
    .FileSystemType     resb 8
endstruc

bpb:
istruc bpb_fat32
    at bpb_fat32.OEMName,           db '        '
    at bpb_fat32.BytesPerSector,    dw 200h
    at bpb_fat32.SectorsPerCluster, db 8
    at bpb_fat32.ReservedSectors,   dw 20h
    at bpb_fat32.FATs,              db 2
    at bpb_fat32.RootEntries,       dw 0
    at bpb_fat32.Sectors,           dw 0
    at bpb_fat32.MediaDescriptor,   db MEDIA_DESCRIPTOR_FIXED_DISK
    at bpb_fat32.FATSectors,        dw 0
    at bpb_fat32.SectorsPerTrack,   dw 3fh
    at bpb_fat32.Heads,             dw 40h
    at bpb_fat32.HiddenSectors,     dd 3fh
    at bpb_fat32.HugeSectors,       db 1, 15h, 26h, 0
    at bpb_fat32.SectorsPerFAT,     dd 2435
    at bpb_fat32.ExtFlags,          dw 0
    at bpb_fat32.FSVersion,         dw 0
    at bpb_fat32.RootCluster,       dd 2
    at bpb_fat32.FSInfoSector,      dw 1
    at bpb_fat32.BackupSector,      dw 6
    at bpb_fat32.Reserved1,         times 12 db 0
    at bpb_fat32.DriveNumber,       db 80h
    at bpb_fat32.Reserved2,         db 0
    at bpb_fat32.BootSignature,     db 29h  ; Indicates the following three fields are valid.
    at bpb_fat32.VolumeID,          dd 0
    at bpb_fat32.VolumeLabel,       db '           '
    at bpb_fat32.FileSystemType,    db 'FAT32   '
iend

;----------------------------------------------------------------------------

after_data:
    cli                             ; Disable interrupts to change the stack frame
                                    ; and interrupt vector table (IVT).  Interrupts
                                    ; are never reenabled.  This may be a bug.

    xor     cx,cx                   ; Set up standard stack and data frames.
    mov     ss,cx
    mov     sp,start - 8

    mov     es,cx                   ; The int 1eh vector is a pointer to the Disk
    mov     bp,1eh * 4              ; Parameter Table.  Real mode interrupt vector
    lds     si,[bp]                 ; table entries are four bytes each.

    push    ds
    push    si
    push    ss
    push    bp

    mov     di,522h                 ; The Disk Parameter Table (DPT) is likely in ROM,
    mov     [bp],di                 ; so copy it out to make it mutable.  The copy is
    mov     [bp + 2],cx             ; placed right after the BIOS data area, in the final
    mov     cl,dpt_size             ; location used by DOS.  Update the IVT entry.
    cld
    repz    movsb

    mov     ds,cx
    mov     bp,start
    mov     byte [di - dpt_size + dpt.HeadSettlingTime],0fh     ; Set the head settling time to 15 ms.

    mov     ax,[bp + (bpb - start) + bpb_fat32.SectorsPerTrack] ; Set the sectors per track in the DPT.
    mov     [di - dpt_size + dpt.SectorsPerTrack],al

    cmp     [bp + (bpb - start) + bpb_fat32.DriveNumber],cl     ; Jump if this is a floppy drive.
    jge     begin_loader_read       ; Fixed disk (80h) would be treated as negative.

    mov     ax,cx                   ; Zero ax and dx.
    cwd
    mov     bx,700h
    call    read_sector_plus_one    ; Read in the MBR (dx:ax=0) to 0:700.
    jc      begin_loader_read

.check_bios_extensions:
    ; bx is the now 0:900, which is the end of the MBR.
    sub     bx,2 + (partition_entry_size * 4) - partition_entry.StartSector

    ; Check if this is the current partition.
    mov     eax,[bpb + bpb_fat32.HiddenSectors]
    cmp     eax,[bx]
    mov     dl,[bx - partition_entry.StartSector + partition_entry.FileSystemType]
    jnz     .no_bios_extensions

    or      dl,2                        ; 0C (32-bit FAT) and 0E (16-bit FAT > 32 MB) use int 13h extensions.
    mov     [bp + bios_extensions],dl   ; Set the BIOS extensions flag.

.no_bios_extensions:
    add     bl,partition_entry_size     ; Go to next partition table entry.
    jnb     .check_bios_extensions      ; Loop until all 4 partition table entries evaluated.

begin_loader_read:
    mov     di,2                ; The number of tries.  One for the first sector, another for the backup.

    cmp     word [bp + (bpb - start) + bpb_fat32.FATSectors],0  ; For FAT32, this must be zero.
    jnz     invalid_disk_msg

    ; Prepare to begin a read immediately following the partition's hidden sectors.
    mov     ax,[bp + (bpb - start) + bpb_fat32.HiddenSectors]
    mov     dx,[bp + (bpb - start) + bpb_fat32.HiddenSectors + 2]
    mov     cx,3                ; Number of sectors to read in (plus one).

.try_finding_a_loader:
    dec     cx
    inc     ax                  ; Sectors are one based.
    jnz     .no_carry           ; Poor man's adc.
    inc     dx

.no_carry:
    mov     bx,end_bootsector   ; Read in the sectors at 0:7e00.
    call    read_sector
    jnc     exit_boot_sector

    mov     al,0f8h             ; This is a meaningless instruction.

    dec     di                  ; Decrement retry counter and exit if all attempts were used.
    jz      print_error_msg

    ; Attempt to read the backup sector, which must be located at least two sectors in.
    ; The two sector requirement is because the FAT32 boot loader occupies the first
    ; two sectors of the disk.
    mov     ax,[bp + (bpb - start) + bpb_fat32.BackupSector]
    xor     dx,dx
    mov     cx,3
    cmp     cx,ax               ; Is the backup sector in a valid location?
    ja      invalid_disk_msg
    mov     si,[bp + (bpb - start) + bpb_fat32.ReservedSectors]
    cmp     cx,si               ; Are there enough reserved sectors?
    jnb     invalid_disk_msg

    sub     si,cx               ; Calculate the count of non-boot record reserved
                                ; sectors for use outside of the boot sector.

    ; Offset the backup sector by the hidden sector count (non-zero on a fixed disk)
    ; to get the logical sector number to read.
    add     ax,[bp + (bpb - start) + bpb_fat32.HiddenSectors]
    adc     dx,[bp + (bpb - start) + bpb_fat32.HiddenSectors + 2]
    jmp     short .try_finding_a_loader

;----------------------------------------------------------------------------

print_error_msg:
    jnb     invalid_disk_msg
    jmp     short disk_io_error_msg

;----------------------------------------------------------------------------

exit_boot_sector:
    cmp     word [bp + (bpb - start) + bpb_fat32.FSVersion],0  ; Only allow FSVersion 0.
    ja      invalid_disk_msg
    jmp     8000h               ; Jump to the new code.

;----------------------------------------------------------------------------

invalid_disk_msg:
    mov     si,emsg_invaliddisk

; si->A pointer to offset from the start of the pointer which is the
; start of the string to display.
print_string:
    lodsb                       ; Get the string offset and add it back
    cbw                         ; into the initial pointer.
    add     si,ax

.next_char:
    lodsb                       ; Get a character.
    test    al,al               ; If it's null, wait for a keystroke.
    jz      wait_for_keystroke
    cmp     al,0ffh             ; If it's -1, append the disk replacement string.
    jz      replace_disk_msg

    mov     ah,0eh
    mov     bx,7
    int     10h                 ; Print this character.
    jmp     short .next_char

;----------------------------------------------------------------------------

replace_disk_msg:
    mov     si,emsg_replacedisk
    jmp     short print_string

;----------------------------------------------------------------------------

disk_io_error_msg:
    mov     si,emsg_diskioerror
    jmp     short print_string

;----------------------------------------------------------------------------

wait_for_keystroke:
    cbw
    int     16h                 ; Wait for a keystroke.

    pop     si                  ; Restore original DPT IVT entry.
    pop     ds
    pop     dword [si]

    int     19h                 ; Reload the boot sector.
                                ; This call does not return.

;----------------------------------------------------------------------------

; es:bx->Pointer to the read buffer.
; dx:ax->Logical sector number to read.
; cx->The number of sectors to read (minus one).
read_sector_plus_one:
    inc     cx                  ; Read one additional sector.

read_sector:
    push    si

    ; Build a disk address packet on the stack:
    ; 00h   BYTE    10h (size of packet).
    ; 01h   BYTE    0 (reserved).
    ; 02h   WORD    Number of sectors to read.
    ; 04h   DWORD   Segment:offset of read buffer.
    ; 08h   QWORD   Starting logical sector number.
    push    dword 0             ; dx:ax is the logical sector number.
    push    dx
    push    ax
    push    es                  ; es:bx is the read buffer.
    push    bx
    push    word 1              ; Number of sectors to read.
    push    word 10h            ; Size of packet.
    mov     si,sp               ; Pointer to the disk address packet (ds:si).
    pusha

    ; Check if int 13h extensions exist.  This check is sketchy,
    ; as this function may be called without this byte set.  It "works"
    ; in that case, as that byte is initialized as a NOP (90h),
    ; and that causes the no-extension path to be called, which is safe.
    cmp     byte [bp + bios_extensions],0eh
    jne     .no_extensions

    mov     ah,42h              ; Extended BIOS Read.
    jmp     short .call_int13

; Convert a logical sector number in dx:ax to CHS format.
.no_extensions:
    xchg    cx,ax
    xchg    dx,ax
    xor     dx,dx

    div     word [bp + (bpb - start) + bpb_fat32.SectorsPerTrack]
    xchg    cx,ax
    div     word [bp + (bpb - start) + bpb_fat32.SectorsPerTrack]
    inc     dx                  ; Sectors are one based.
    xchg    cx,dx
    div     word [bp + (bpb - start) + bpb_fat32.Heads]
    mov     dh,dl               ; Final head number goes in dh.
    mov     ch,al               ; Put the cylinder in ch, with the high
    ror     ah,2                ; bits (8-9) in the top of cl.  The lower six
    or      cl,ah               ; bits hold the starting sector number.

    mov     ax,201h             ; Command to read one sector.

.call_int13:
    mov     dl,[bp + (bpb - start) + bpb_fat32.DriveNumber]
    int     13h                 ; Read the sector.

    popa
    lea     sp,[si + 10h]       ; Release the disk address packet.
    pop     si

    jc      .exit

    inc     ax                  ; Increment to the next logical sector.
    jnz     .no_carry
    inc     dx

.no_carry:
    add     bx,[bp + (bpb - start) + bpb_fat32.BytesPerSector]  ; Offset read buffer by bytes read.
    dec     cx                  ; Decrement loop counter once for each sector read.
    jnz     read_sector

.exit:
    retn

; String Table --------------------------------------------------------------

emsg_invaliddisk:
db invalid_disk - $ - 1

emsg_diskioerror:
db diskioerror - $ - 1

emsg_invaliddisk_dupe:
db invalid_disk - $ - 1

emsg_replacedisk:
db replaced_disk - $ -1

invalid_disk:   db 0dh, 0ah, 'Invalid system disk', 0ffh
diskioerror:    db 0dh, 0ah, 'Disk I/O error', 0ffh
replaced_disk:  db 0dh, 0ah, 'Replace the disk, and then press any key', 0dh, 0ah, 0

dw 0
db 'IO      SYS'
;dw 0
db 'MSDOS   SYS'
db 7eh, 1, 0
db 'WINBOOT SYS'
dw 0

    dw 0aa55h
end_bootsector:

; bios_extensions flag is overlayed over the 2nd code byte of the boot sector.
absolute 0
                resw 1
bios_extensions resb 1

