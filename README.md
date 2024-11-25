# Genome Sequence REPL

All living entities possess genetic information which determines most traits and\
characteristics of an organism. Nucleotides are the building blocks of DNA and\
RNA, and by analyzing sequences of them, we can gain an understanding of the\
genetic code. This REPL program helps with such tasks.

## Entities

- **Nucleotides**: Represented by the letters A (adenine), T (thymine), C (cytosine), and G (guanine).
- **Sequence**: A string of nucleotides (e.g., "ATCG").

## Operations

- **Concatenate**: Combines two nucleotide sequences into one.\
  Example: `concat ATCG GCTA` results in `ATCGGCTA`.

- **Find Motif**: Finds the location of a certain string of nucleotides within a larger sequence.\
  Example: `fmotif ATCGGCTA GCTA` finds `GCTA` within `ATCGGCTA` (index 4).

- **Complement**: Computes the complementary nucleotide sequence (A -> T, T -> A, C -> G, G -> C).\
  Example: `complement ATCG` results in `TAGC`.

- **Transcribe**: Converts a DNA sequence into its RNA equivalent (A -> U, T -> A, C -> G, G -> C).\
  Example: `transcribe ATCG` results in `UAGC`.

- **Mutate**: Takes a sequence and a percentage to randomly change that percentage of nucleotides.\
  Example: `mutate ATC 30` could result in `ATG`.

- **CreateSeq**: Allows the user to create named sequences for use later.\
  Example: `createseq GGT mySeq1`

- **DeleteSeq**: Allows the user to create named sequences for use later.\
  Example: `deleteseq mySeq1`

- **SaveTo**: Allows the user to store the result of commands that are performed in a sequence.\
  Example: `saveTo mySeq1 concat (complement GGG) AT` 
  would result in a new sequence named mySeq1 that hols CCCAT

## Combining Commands

Commands can be combined to perform more complex operations.\
Example: `fmotif (concat ATCG GCTA) TCG` finds the motif TCG in ATCGGCTA.

## Saving State

Performed commands and saved sequences can be saved into a file and loaded from a file using
`save` and `load`

<br><br>
 ## BNF
```bnf
<nucleotide> ::= "A" | "T" | "C" | "G"
<sequence> ::= <nucleotide> | <nucleotide> <sequence>
<namedsequence> ::= <string>


<string> ::= <character> <string> | <character>
<integer> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" | <integer> <integer>
<percentage> ::= <integer>
 
<operand> ::= <sequence> | <operation> | <namedsequence>
<operation> ::= "concat" <operand> <operand>
              | "fmotif" <operand> <operand>
              | "complement" <operand>
              | "transcribe" <operand>
              | "mutate" <operand> <percentage>
              | "createSeq" <sequence> <string>
              | "deleteSeq" <string>
              | "view"
              | "saveTo" <string> <operation>
              
<nestedoperation> ::= "(" <operation> ")"

<character> ::= "a" | "b" | "c" | "d" | "e" | "f" | "g" | "h" | "i" | "j" | "k" |
                "l" | "m" | "n" | "o" | "p" | "q" | "r" | "s" | "t" | "u" | "v" | "w" | "x" |
                "y" | "z" | "A" | "B" | "C" | "D" | "E" | "F" | "G" | "H" | "I" | "J" | "K" |
                "L" | "M" | "N" | "O" | "P" | "Q" | "R" | "S" | "T" | "U" | "V" | "W" | "X" |
                "Y" | "Z" | "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"


