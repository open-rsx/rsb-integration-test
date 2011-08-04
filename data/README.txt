The file event-id-cases.txt contains test case data for the event id
computation mechanism that is used in RSB (see
https://code.cor-lab.org/projects/rsb/wiki/Events#Events for details)

The contents of the file consists of 1024 lines of the form

ORIGIN-UUID SEQUENCE-NUMBER EVENT-UUID

SEQUENCE-NUMBER is a random unsigned 32-bit integer which is printed
using radix 16 and zero-padding to width 8.

For each line, feeding ORIGIN-UUID and the string representation of
SEQUENCE-NUMBER into an UUIDv5 generator should produce EVENT-UUID.
