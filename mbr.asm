; Disassembly of a Master Boot Record, by Toby Jones.
; Disassembled around 16 March 2001.

;----------------------------------------------------------------------------
; The code is loaded at 0:7c00, but the boot code relocates itself to 0:600.

org 600h

start_mbr:
    cli                             ; Set up a stack frame at 0:7c00.
    xor     ax,ax
    mov     ss,ax
    mov     sp,7c00h
    mov     si,sp
    push    ax                      ; Set the segment registers to 0.
    pop     es
    push    ax
    pop     ds
    sti

    ; Relocate the boot code to 0:600, so that the partition's boot
    ; sector can be loaded back to 0:7c00.
    cld
    mov     di,600h
    mov     cx,(end_mbr-start_mbr) / 2
    repnz   movsw
    jmp     0000:relocation         ; Jump to new location of boot code.

; Relocation entry point ----------------------------------------------------

relocation:
    mov     si,entry1
    mov     bl,4                    ; Four partition table entries.
.next_entry:
    cmp     byte [si],80h           ; Look for bootable flag.
    jz      found_boot_entry
    cmp     byte [si],0             ; Look for non-bootable flag.
    jnz     print_invalid_table
    add     si,partition_entry_size ; Go to start of next table entry.
    dec     bl                      ; Decrement count of entries to try.
    jnz     .next_entry

    ; If a bootable table entry was not found, go to ROM Basic.
    int     18h                     ; ROM Basic.

;----------------------------------------------------------------------------

; si->Bootable partition table entry.
; bl->Entries left to scan + 1.
found_boot_entry:
    mov     dx,[si]                               ; Get drive and head.
    mov     cx,[si + partition_entry.BeginSector] ; Get sector and cylinder.
    mov     bp,si                                 ; Save pointer to bootable entry.

.next_entry:
    add     si,partition_entry_size               ; Get pointer to next table entry.
    dec     bl                                    ; Finish booting if at the end of the table.
    jz      finish_boot
    cmp     byte [si],00        ; Is this new entry bootable?
    jz      .next_entry         ; If it is, then there is a problem (2 bootable entries).

print_invalid_table:
    mov     si,emsg_invalid_table

; si->The string to print.
print_string_and_halt:
    lodsb                       ; Get a character.
    cmp     al,0                ; If the character is null, then finish.
    jz      halt

    push    si                  ; Save string pointer (likely to work around old BIOS bugs).
    mov     bx,7
    mov     ah,0eh
    int     10h                 ; Print this character.
    pop     si                  ; Restore string pointer.

    jmp     short print_string_and_halt

halt:
    jmp     short $             ; Halt the system via infinite loop.

;----------------------------------------------------------------------------

; cx->Cylinder and sector to read.
; dx->Drive and head to read.
finish_boot:
    mov     di,5                ; Max tries to read disk.
.read_boot_sector:
    mov     bx,7c00h            ; Read to 0:7c00.
    mov     ax,201h
    push    di
    int     13h                 ; Read in the first sector of partition.
    pop     di
    jnc     read_success        ; If read is successful, then try to jump to it.

    xor     ax,ax
    int     13h                 ; Reset disk.
    dec     di                  ; Decrement try count.
    jnz     .read_boot_sector

    mov     si,emsg_load_error  ; 'Error loading operating system'
    jmp     short print_string_and_halt

;----------------------------------------------------------------------------

read_success:
    mov     si,emsg_missing_os      ; 'Missing operating system'
    mov     di,7dfeh
    cmp     word [di],0aa55h        ; Make sure it's a valid boot sector.
    jnz     print_string_and_halt   ; If not, print error message.

    mov     si,bp                   ; Pass partition table entry as ds:si.
    jmp     0000:7C00h              ; Jump to start of boot sector.

; String Table --------------------------------------------------------------

emsg_invalid_table:
    db 'Invalid partition table', 0
emsg_load_error:
    db 'Error loading operating system', 0
emsg_missing_os:
    db 'Missing operating system', 0

    times 227 db 0      ; padding

; Partition Table -----------------------------------------------------------

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

entry1:
    istruc partition_entry
    iend
entry2:
    istruc partition_entry
    iend
entry3:
    istruc partition_entry
    iend
entry4:
    istruc partition_entry
    iend

    dw 0aa55h
end_mbr:

