#!/bin/bash

random_string() {
    local length=$1
    cat /dev/urandom | tr -dc 'a-zA-Z0-9' | fold -w $length | head -n 1
}

random_content() {
    local type=$1
    case $type in
        "txt")
            fortune 2>/dev/null || echo "Random text: $(random_string 100)"
            ;;
        "json")
            echo "{\"id\": $RANDOM, \"name\": \"$(random_string 10)\", \"value\": $RANDOM}"
            ;;
        "csv")
            echo "id,name,value"
            for i in {1..5}; do
                echo "$RANDOM,$(random_string 8),$RANDOM"
            done
            ;;
        "html")
            cat <<EOF
<!DOCTYPE html>
<html>
<head><title>$(random_string 10)</title></head>
<body>
    <h1>Random Page $(random_string 5)</h1>
    <p>Content: $(random_string 50)</p>
</body>
</html>
EOF
            ;;
        "md")
            echo "# $(random_string 15)"
            echo "## Section $(random_string 10)"
            echo "Random content: $(random_string 100)"
            ;;
        "sh")
            echo "#!/bin/bash"
            echo "# Random script"
            echo "echo \"Hello from $(random_string 10)\""
            ;;
        *)
            random_string 200
            ;;
    esac
}

extensions=("txt" "json" "csv" "html" "md" "sh" "log" "conf" "xml" "py")

create_structure() {
    local current_dir=$1
    local depth=$2
    local max_depth=$3

    local num_files=$((RANDOM % 7 + 1))

    for ((i=1; i<=num_files; i++)); do
        local filename="$(random_string 8)"
        local ext=${extensions[$RANDOM % ${#extensions[@]}]}
        local filepath="$current_dir/${filename}.${ext}"

        random_content "$ext" > "$filepath"
        echo "Создан файл: $filepath"
    done

    if [ $depth -lt $max_depth ]; then
        local num_dirs=$((RANDOM % 4))

        for ((j=1; j<=num_dirs; j++)); do
            local dirname="dir_$(random_string 6)"
            local dirpath="$current_dir/$dirname"

            mkdir -p "$dirpath"
            echo "Создана директория: $dirpath"

            create_structure "$dirpath" $((depth + 1)) $max_depth
        done
    fi
}

main() {
    ROOT_DIR="TEST_DIR"

    MAX_DEPTH=$((RANDOM % 9 + 1))

    echo "==================================="
    echo "Создание случайной структуры..."
    echo "Корневая директория: $ROOT_DIR"
    echo "Максимальная глубина: $MAX_DEPTH"
    echo "==================================="

    mkdir -p "$ROOT_DIR"

    create_structure "$ROOT_DIR" 1 $MAX_DEPTH

    cat > "$ROOT_DIR/README.md" <<EOF
# Random Project $(random_string 10)

This is an automatically generated project structure.

- Generated on: $(date)
- Max depth: $MAX_DEPTH
- Root directory: $ROOT_DIR

## Structure

\`\`\`
$(tree "$ROOT_DIR" 2>/dev/null || find "$ROOT_DIR" -type f | head -20)
\`\`\`
EOF

    echo "==================================="
    echo "Структура создана успешно!"
    echo "Директория проекта: $ROOT_DIR"
    echo "==================================="

    echo "$ROOT_DIR" > .last_random_project
}

main