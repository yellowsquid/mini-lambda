#!/usr/bin/bash

function realpath() {
    [[ $1 = /* ]] && echo "$1" || echo "$PWD/${1#./}"
}

function cleanup() {
  rm -rf "$WORK_DIR"
}

# A POSIX variable
OPTIND=1

# Find the directory where the script is located.
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

# Parse the arguments.
OUTPUT=""
INTERP_LEVEL=""
while getopts "h?gi:o:" opt; do
    case "$opt" in
    h|\?)
        show_help
        exit 0
        ;;
    g)
        FLAGS="${FLAGS:+$FLAGS }-g"
        ;;
    i)
        FLAGS="${FLAGS:+$FLAGS }-i $OPTARG"
        INTERP_LEVEL=$OPTARG
        ;;
    o)
        OUTPUT=$OPTARG
        ;;
    esac
done

shift $((OPTIND-1))

[ "${1:-}" = "--" ] && shift

if test -n "$OUTPUT" && test -n "$INTERP_LEVEL"; then
    echo "Cannot compile and interpret."
    exit 9
fi

CC=${CC:-gcc}

make release

EXEC="$DIR/_build/install/default/bin/lambda"

# Find the input file.
INPUT=$(realpath "$1")
if test -f "$INPUT"; then
  # Compile the file.
  case "$OUTPUT" in
    "")
      # Dump assembly to stdout.
        if ! $EXEC $FLAGS "$INPUT"; then
            echo "Failed to compile."
            exit 1
        fi
      ;;
    *)
      # Create a temporary directory.
      WORK_DIR=${WORKDIR:-$(mktemp -d)}

      # Compile and link.
      if ! (
              $EXEC $FLAGS "$INPUT" -o "$WORK_DIR/lambda.S"
              $CC -c runtime/runtime.c -o "$WORK_DIR/runtime.o"
              $CC $FLAGS "$WORK_DIR/lambda.S" "$WORK_DIR/runtime.o" -o "$OUTPUT"
          ); then
          echo "Failed to build."
          exit 1
      fi

      # Cleanup temp directory.
      if test -z $FLAGS; then
          trap cleanup EXIT
      fi
      ;;
  esac
else
  echo "File does not exist: $INPUT"
fi
