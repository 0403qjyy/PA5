#!/bin/bash
# 生成汇编文件的脚本
# 使用方法: ./generate_asm.sh input.cl output.s

INPUT=$1
OUTPUT=$2

if [ -z "$INPUT" ] || [ -z "$OUTPUT" ]; then
    echo "Usage: $0 input.cl output.s"
    exit 1
fi

echo "Generating assembly file: $OUTPUT from $INPUT"
./lexer "$INPUT" 2>/dev/null | ./parser "$INPUT" 2>/dev/null | ./semant "$INPUT" 2>/dev/null | ./cgen -o "$OUTPUT" "$INPUT" 2>&1

if [ -f "$OUTPUT" ]; then
    echo "Success! Generated $OUTPUT"
    echo "File size: $(ls -lh "$OUTPUT" | awk '{print $5}')"
else
    echo "Failed to generate $OUTPUT"
    exit 1
fi
