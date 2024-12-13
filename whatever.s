// Assembler program to print "Hello World!"
// to stdout.
//
// X0-X2 - parameters to linux function services
// X16 - linux function number
//
.global _start             // Provide program starting address to linker
.p2align 3 // Feedback from Peter

_start:
    mov     X0, #0      // Use 0 return code
    mov     X16, #1     // Service command code 1 terminates this program
    svc     0           // Call MacOS to terminate the program

// Setup the parameters to print hello world
// and then call Linux to do it.
; print_hello_world: mov X0, #1     // 1 = StdOut
;         adr X1, helloworld // string to print
;         mov X2, #13  // length of our string
;         mov X16, #4     // MacOS write system call
;         svc 0     // Call linux to output the string
;         ret

; print_goodbye: mov X0, #1     // 1 = StdOut
;         adr X1, goodbyeworld // string to print
;         mov X2, #13  // length of our string
;         mov X16, #4     // MacOS write system call
;         svc 0     // Call linux to output the string
;         ret

; _start: MOV X10, 1
;         MOV X11, 4
;         MOV X12, helloworld
;         MOV X13, goodbyeworld
;         ADD X9, X10, X11
;         CMP X9, 3
;         csel X12, X14, X13, EQ

;         ; B.EQ print_hello_world
;         ; B.NE print_goodbye

;         mov X0, #1     // 1 = StdOut
;         adr X1, X14 // string to print`
;         mov X2, #13  // length of our string
;         mov X16, #4     // MacOS write system call
;         svc 0     // Call linux to output the string
/*
mov X0, #1     // 1 = StdOut
        adr X1, 123 // string to print
        mov X2, #3     // length of our string
        mov X16, #4     // MacOS write system call
        svc 0     // Call linux to output the string

*/
// Setup the parameters to exit the program
// and then call Linux to do it.

helloworld:      .ascii  "Hello World!\n" // 13
goodbyeworld:   .ascii "Good World!\n" // 13