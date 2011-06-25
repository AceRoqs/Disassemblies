; Disassembly of a Microsoft NTFS volume boot record, by Toby Jones.
; Disassembled around 16 March 2001.

; An interesting thing about this boot sector is that it uses 386+ instructions.
; This is okay, as this code will be installed as the boot sector on the boot
; partition of a fixed disk, so there is little risk of the code executing
; on an unsupported platform.

; This code is relevant up through Windows XP.  In Vista and newer versions,
; NTLDR was replaced with winload.exe and the Windows Boot Manager.

;----------------------------------------------------------------------------
; The code is loaded by the BIOS to 0:7c00, but data is referenced from 7c0:0.

org 0

start:
    jmp     short after_data
    nop

; BIOS Parameter Block ------------------------------------------------------

%define MEDIA_DESCRIPTOR_FIXED_DISK 0f8h

struc bpb_ntfs
    .OEMName                      resb 8
    .BytesPerSector               resw 1
    .SectorsPerCluster            resb 1
    .ReservedSectors              resw 1
    .Reserved1                    resb 5
    .MediaDescriptor              resb 1
    .Reserved2                    resw 1
    .SectorsPerTrack              resw 1
    .Heads                        resw 1
    .HiddenSectors                resd 1
    .Reserved3                    resd 1
    .Reserved4                    resd 1
    .TotalSectors                 resd 2
    .MFTCluster                   resd 2
    .MFTMirrorCluster             resd 2
    .ClustersPerFileRecordSegment resd 1
    .ClustersPerIndexBlock        resd 1
    .Serial                       resd 2
    .Checksum                     resd 1
endstruc

bpb:
istruc bpb_ntfs
    at bpb_ntfs.OEMName,                      db 'NTFS    '
    at bpb_ntfs.BytesPerSector,               dw 200h
    at bpb_ntfs.SectorsPerCluster,            db 1
    at bpb_ntfs.ReservedSectors,              dw 0
    at bpb_ntfs.Reserved1,                    times 5 db 0
    at bpb_ntfs.MediaDescriptor,              db MEDIA_DESCRIPTOR_FIXED_DISK
    at bpb_ntfs.Reserved2,                    dw 0
    at bpb_ntfs.SectorsPerTrack,              dw 3fh
    at bpb_ntfs.Heads,                        dw 0ffh
    at bpb_ntfs.HiddenSectors,                dd 3fh
    at bpb_ntfs.Reserved3,                    dd 0
    ; Reserved4 is used by the boot code to store the boot disk number.
    at bpb_ntfs.Reserved4,                    dd 80008000h
    at bpb_ntfs.TotalSectors,                 dd 0,0
    at bpb_ntfs.MFTCluster,                   dd 0,0
    at bpb_ntfs.MFTMirrorCluster,             dd 0,0
    at bpb_ntfs.ClustersPerFileRecordSegment, dd 2
    at bpb_ntfs.ClustersPerIndexBlock,        dd 8
    at bpb_ntfs.Serial,                       dd 0,0
    at bpb_ntfs.Checksum,                     dd 0
iend

; Global Variables ----------------------------------------------------------

next_logical_sector dd 0    ; Next logical sector to read.
read_cylinder       dw 0    ; CHS cylinder number to read.
read_sector         db 0    ; CHS sector number to read.
sectors_left        dw 0    ; Number of sectors left to read.

;----------------------------------------------------------------------------

after_data:
    cli                             ; Set up standard stack and data frames.
    xor     ax,ax                   ; Stack is at 0:7c00, Data is at 7c0:0.
    mov     ss,ax
    mov     sp,7c00h
    sti
    mov     ax,7c0h
    mov     ds,ax

    mov     word [next_logical_sector],0      ; Set first logical sector.
    mov     word [next_logical_sector + 2],0  ; Set high word of first sector.
    mov     word [sectors_left],16            ; Minimum NTLDR sectors to read.

    mov     ax,0d00h                ; Load NTLDR to d00:0.
    mov     es,ax
    sub     bx,bx
    call    load_NTLDR

    push    0d00h                   ; Jump to NTLDR entry point at d00:266.
    push    266h
    retf

;----------------------------------------------------------------------------

load_NTLDR:
    push    ax
    push    bx
    push    cx
    push    dx
    push    es

.next_track:
    ; Convert a logical sector to CHS.
    mov     eax,[next_logical_sector]           ; Get logical sector start and
    add     eax,[bpb + bpb_ntfs.HiddenSectors]  ; add in hidden sector count.
    xor     edx,edx
    movzx   ecx,word [bpb + bpb_ntfs.SectorsPerTrack]
    div     ecx                                 ; Divide by sectors per track (get cylinder).
    inc     dl                                  ; Sectors are one based.
    mov     [read_sector],dl                    ; Remainder is the starting sector number.

    mov     edx,eax                             ; Convert cylinder from
    shr     edx,16                              ; 32-bit number to 16:16 pair (dx:ax).
    div     word [bpb + bpb_ntfs.Heads]         ; Divide by number of heads
    mov     [bpb + bpb_ntfs.Reserved4 + 1],dl   ; Save resulting head.
    mov     [read_cylinder],ax                  ; Save resulting cylinder.

    ; Calculate number of sectors to read in.
    mov     al,[bpb + bpb_ntfs.SectorsPerTrack] ; Subtract the starting sector from the sectors per
    sub     al,[read_sector]                    ; track to get the count of sectors to read.  Reads
    inc     ax                                  ; are guaranteed to not cross track/cylinder boundaries.
    cmp     ax,[sectors_left]                   ; Only read in the remaining sectors of NTLDR.
    jbe     load_NTLDR.above_min
    mov     ax,[sectors_left]

; al->Number of sectors to read.
; es:bx->Pointer to the read buffer (d00:0).
.above_min:
    push    ax
    mov     ah,2                                ; Put the cylinder in ch, with the high
    mov     dx,[read_cylinder]                  ; bits (8-9) in the top of cl.  The lower six
    mov     cl,6                                ; bits hold the starting sector number.
    shl     dh,cl
    or      dh,[read_sector]
    mov     cx,dx
    xchg    ch,cl
    mov     dh,[bpb + bpb_ntfs.Reserved4 + 1]   ; Put the head number in dh, and the drive number
    mov     dl,80h                              ; in dl (80h is the first fixed disk).
    int     13h                                 ; Read in the sectors.

    pop     ax
    jc      disk_read_error
           
    add     [next_logical_sector],ax            ; Offset by the number of sectors just read.
    adc     word [next_logical_sector + 2],0
    sub     [sectors_left],ax                   ; Reduce the count of sectors left to read.
    jbe     pop_and_return                      ; Jump if enough sectors read.

    ; Move the destination pointer by the amount read.  The bx register is unchanged,
    ; but es is offset by the bytes read / 16 (since each segment is offset by 16 bytes).
    ; Assuming 512 byte sectors, offset the segment register by the number of sectors * 32.
    ; (32 * 16 = 512).
    shl     ax,5                                ; Multiply sector count by 32 (512/16).
    mov     dx,es
    add     dx,ax                               ; Offset segment by bytes read / 16.
    mov     es,dx
    jmp     short .next_track

pop_and_return:
    pop     es
    pop     dx
    pop     cx
    pop     bx
    pop     ax
    retn

;----------------------------------------------------------------------------

; This block of code has no entry point in the boot sector.
; It is possible that this code is exported to NTLDR.

    mov     si,emsg_kernel_missing      ; 'A kernel file is missing'...
    jmp     short insert_disk_and_halt

    mov     si,emsg_ntldr_compressed    ; '\NTLDR is compressed.'
    jmp     short insert_disk_and_halt

disk_read_error:
    mov     si,emsg_read_error          ; 'A disk read error occurred.'

insert_disk_and_halt:
    call    print_string

    mov     si,emsg_insert_disk         ; 'Insert a system diskette'...
    call    print_string
    sti
    jmp     short $                     ; Halt the system via infinite loop.

;----------------------------------------------------------------------------

; si->The string to print.
print_string:
    lodsb                           ; Get a character.
    cmp     al,0                    ; If the character is null, then return.
    jz      .exit

    mov     ah,0eh
    mov     bx,7
    int     10h                     ; Print this character.
    jmp     short print_string      ; Go do the next character.

.exit:
    retn

; String Table --------------------------------------------------------------

; The strings in this table are both null terminated (C style) and prefixed
; with the count (Pascal or OLE style).  Many of the strings go unused in
; the boot sector.  The C style strings are used by print_string(), and
; the prefixed strings could possibly be used by NTLDR.

    dw emsg_read_error.emsg_end - emsg_read_error - 1
emsg_read_error:
    db 'A disk read error occurred.', 0dh, 0ah, 0
.emsg_end:

; This string is never accessed even though there is code for it above.
    dw emsg_kernel_missing.emsg_end - emsg_kernel_missing - 1
emsg_kernel_missing:
    db 'A kernel file is missing from the disk.', 0dh, 0ah, 0
.emsg_end:

; This is never referenced in the boot sector.
    dw emsg_kernel_fragmented.emsg_end - emsg_kernel_fragmented - 1
emsg_kernel_fragmented:
    db 'A kernel file is too discontiguous.', 0dh, 0ah, 0
.emsg_end:

    dw emsg_insert_disk.emsg_end - emsg_insert_disk - 1
emsg_insert_disk:
    db 'Insert a system diskette and restart the system.', 0dh, 0ah, 0
.emsg_end:

; This string is never accessed even though there is code for it above.
; The slash at the beginning of the string is literal from the boot sector.
    dw emsg_ntldr_compressed.emsg_end - emsg_ntldr_compressed - 1
emsg_ntldr_compressed:
    db '\NTLDR is compressed.', 0dh, 0ah, 0
.emsg_end:

    dd 0
    dw 0aa55h

