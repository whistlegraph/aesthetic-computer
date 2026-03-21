# Research: Alias for `repeat` in KidLisp

**Date:** October 1, 2025  
**Goal:** Find a shorter, more ergonomic alias for the `repeat` word in KidLisp

## Background

The current `repeat` word is functional but at 6 characters, it's a bit long for such a commonly used construct. We want to add an alias that's:
- Shorter and more ergonomic
- Kid-friendly and intuitive
- Respectful of Lisp traditions (where applicable)
- Fun and fitting with KidLisp's playful aesthetic

## Lisp Dialect Survey

### Common Lisp
- **`loop`** - Extremely powerful macro with complex DSL syntax
- **`dotimes`** - Standard form: `(dotimes (var count) body)`
- **`do`** - General iteration with complex binding syntax
- **`dolist`** - Iterate over lists

### Scheme
- **`do`** - General iteration macro
- **`dotimes`** - Available in some implementations (not standard)
- **`repeat`** - Used in SRFI-42 (Eager Comprehensions)
- Named `let` - Common idiom for loops via recursion

### Clojure
- **`dotimes`** - `(dotimes [i n] body)` - iterate n times
- **`doseq`** - Sequence iteration
- **`loop`/`recur`** - Tail-recursive iteration

### Logo (Kid-Friendly Language)
- **`repeat`** - `repeat 10 [forward 50 right 36]`
- This is exactly what KidLisp currently uses!

### Other Languages
- **`times`** - Ruby uses this: `10.times { ... }`
- **`for`** - Traditional in many languages but not very Lisp-y
- **`each`** - Usually implies iterating over a collection

## Proposed Options

### 1. `times` ⭐ **RECOMMENDED**
**Pros:**
- Natural English: "do this 10 times"
- 5 characters (1 shorter than repeat)
- Kid-friendly and intuitive
- Used in Ruby with similar meaning
- Reads well: `(times 100 (box x y 10 10))`

**Cons:**
- Not traditional Lisp terminology
- Could potentially conflict with multiplication context

### 2. `dotimes`
**Pros:**
- Classic Lisp terminology (Common Lisp, Clojure)
- Well-established meaning
- Unambiguous

**Cons:**
- Actually LONGER than repeat (7 chars vs 6)
- Defeats the purpose of an alias

### 3. `rep`
**Pros:**
- Shortest option (3 characters)
- Clear abbreviation of repeat
- Very ergonomic

**Cons:**
- Feels a bit too abbreviated
- Could be confused with "representative" or other meanings
- Less intuitive for kids

### 4. `bunch`
**Pros:**
- Whimsical and fun
- Fits KidLisp aesthetic
- 5 characters
- Unique to KidLisp

**Cons:**
- No precedent in other languages
- "Bunch" typically means "a group" not "multiple times"
- Could be semantically confusing

### 5. `loop`
**Pros:**
- 4 characters - very short
- Universal concept
- Common Lisp uses it

**Cons:**
- In Common Lisp, `loop` is a complex macro with DSL
- Might set wrong expectations
- Generic term that could be used for other loop types later

## Recommendation: FINAL

**Initial recommendation was `times`, but that reads backward. Then `rep`, but that's not a full word.**

### Final Recommendation: **`bunch`** ⭐

**`bunch`** is the perfect choice because:

1. **Contextual meaning**: "Bunching together" Lisp expressions/clods - captures the essence of repetition in a Lisp context
2. **Playful and unique**: Fits KidLisp's whimsical, creative aesthetic perfectly
3. **Shorter**: 5 characters vs 6 for repeat
4. **Complete word**: Real word that's kid-friendly and intuitive
5. **Natural ordering**: "bunch 10" reads well - "bunch 10 (of these together)"
6. **Unambiguous**: Won't conflict with other potential language features

### Why not other options:

- **`times`**: Reads backward "(times 10)" vs English "10 times" ✗
- **`rep`**: Not a complete word, abbreviation isn't intuitive ✗
- **`loop`**: Generic, less creative than bunch
- **`dotimes`**: Longer than repeat (defeats the purpose)
- Keep **`repeat`**: Valid but we want something shorter and more fun

## Implementation Plan

1. Add `bunch` as an alias to `repeat` in the specialForms object
2. Update documentation to mention both forms
3. Keep `repeat` as primary documentation form (Logo tradition, fully spelled out)
4. Both `repeat` and `bunch` will work identically

## Example Usage

```lisp
; Current syntax (still works, recommended for documentation)
(repeat 100 (box (wiggle width) (wiggle height) 10 10))

; New shorter alias - "bunching together" code clods!
(bunch 100 (box (wiggle width) (wiggle height) 10 10))

; With iterator - both forms
(repeat height i
  (ink rainbow)
  (line 0 i width i))

(bunch height i
  (ink rainbow)
  (line 0 i width i))
```

## Philosophy

`bunch` perfectly captures the Lisp philosophy of "bunching together" expressions. When you use `(bunch 10 ...)`, you're literally creating a bunch of 10 instances of those code clods, executed together. It's playful, memorable, and uniquely KidLisp!
