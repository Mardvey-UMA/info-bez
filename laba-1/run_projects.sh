#!/bin/bash

# run_project.sh

if [ ! -f .last_random_project ]; then
    echo "Ошибка: Сначала создайте проект с помощью ./create_random_structure.sh"
    exit 1
fi

PROJECT_DIR=$(cat .last_random_project)

if [ ! -d "$PROJECT_DIR" ]; then
    echo "Ошибка: Директория проекта не найдена"
    exit 1
fi

echo "==================================="
echo "Запуск проекта: $PROJECT_DIR"
echo "==================================="

# Имитация запуска проекта
cd "$PROJECT_DIR"

# Показываем структуру
echo "Структура проекта:"
tree . 2>/dev/null || ls -la

echo ""
echo "Содержимое README.md:"
cat README.md 2>/dev/null || echo "README.md не найден"

echo ""
echo "==================================="
echo "Проект 'запущен' в директории: $(pwd)"
echo "==================================="

# Открываем новый shell в директории проекта
exec $SHELL