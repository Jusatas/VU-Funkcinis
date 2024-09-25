# Genome Sequence REPL

All living entities possess genetic information which determines most traits and\
characteristics of an organism. Nucleotides are the building blocks of DNA and\
RNA, and by analyzing sequences of them, we can gain an understanding of the\
genetic code. This REPL program helps with such tasks.

## Entities

- **Nucleotides**: Represented by the letters A (adenine), T (thymine), C (cytosine), and G (guanine).
- **Sequence**: A string of nucleotides (e.g., "ATCG").
- **Chromosome**: A collection of sequences (e.g., "ATCG" "GCTA").
- **Genome**: A collection of chromosomes (e.g., "ATCG" "GCTA" "CGTA").

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

## Combining Commands

Commands can be combined to perform more complex operations.\
Example: `find-motif (concat ATCG GCTA) TCG` finds the motif TCG in ATCGGCTA.

<br><br>
 ## BNF
```bnf
<nucleotide> ::= "A" | "T" | "C" | "G"
<sequence> ::= <nucleotide> | <nucleotide> <sequence>
 
<chromosome> ::= <sequence> | <sequence> <chromosome>
<genome> ::= <chromosome> | <chromosome> <genome>
 
<integer> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" | <integer> <integer>
<percentage> ::= <integer>
 
<operand> ::= <sequence> | <operation>
<operation> ::= "concat" <operand> <operand>
              | "find-motif" <operand> <operand>
              | "complement" <operand>
              | "transcribe" <operand>
              | "mutate" <operand> <percentage>
