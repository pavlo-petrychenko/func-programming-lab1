#!/usr/bin/env bash
# Димовий тест: проходить повний цикл створення / читання / редагування / пошуку /
# видалення через HTTP-інтерфейс запущеного застосунку з реальною MySQL.
#
# Використання:  BASE_URL=http://localhost:3000 ./scripts/smoke.sh
set -u
BASE_URL="${BASE_URL:-http://localhost:3000}"
SUFFIX="$(date +%s)"
PASS=0
FAIL=0

ok()   { PASS=$((PASS + 1)); echo "  ok   - $1"; }
fail() { FAIL=$((FAIL + 1)); echo "  FAIL - $1"; }

# check "опис" "текст, що має бути у відповіді" "вміст відповіді"
contains() { if grep -qF -- "$2" <<<"$3"; then ok "$1"; else fail "$1 (немає \"$2\")"; fi; }
status_is() { if [ "$2" = "$3" ]; then ok "$1"; else fail "$1 (код $3, очікувався $2)"; fi; }

get()  { curl -s "$BASE_URL$1"; }
code() { curl -s -o /dev/null -w '%{http_code}' "$@"; }
# post_form шлях поле=значення ...  -> тіло відповіді
post_form() {
  local path="$1"; shift
  local args=()
  for kv in "$@"; do args+=(--data-urlencode "$kv"); done
  curl -s -i "${args[@]}" "$BASE_URL$path"
}
# id першого запису у списку, знайденого за унікальним текстом
find_id() { # таблиця текст
  curl -s -G --data-urlencode "q=$2" "$BASE_URL/t/$1" | grep -o "/t/$1/[0-9]*/edit" | head -1 | grep -o '[0-9]\+'
}
location() { grep -i '^location:' <<<"$1" | tr -d '\r' | awk '{print $2}'; }
httpcode() { head -1 <<<"$1" | awk '{print $2}'; }

echo "== Базова доступність"
contains "головна сторінка відкривається" "Таблиці бази даних" "$(get /)"
for t in service_kinds authors services service_authors service_versions usage_terms users registrations usage_stats; do
  status_is "таблиця $t доступна" 200 "$(code "$BASE_URL/t/$t")"
done

echo "== Створення (INSERT)"
r=$(post_form /t/service_kinds "name=Тестовий вид $SUFFIX" "description=для смоук-тесту")
status_is "вид сервісу створено (redirect)" 302 "$(httpcode "$r")"
KIND=$(find_id service_kinds "Тестовий вид $SUFFIX")
[ -n "$KIND" ] && ok "знайдено id виду: $KIND" || fail "id виду не знайдено"

r=$(post_form /t/authors "full_name=Смоук Автор $SUFFIX" "email=smoke$SUFFIX@fac.example.edu" "position=тестер" "department=")
status_is "автора створено" 302 "$(httpcode "$r")"
AUTHOR=$(find_id authors "smoke$SUFFIX@fac")

r=$(post_form /t/services "name=Смоук-сервіс $SUFFIX" "annotation=Сервіс для перевірки створення та пошуку" \
  "kind_id=$KIND" "current_version=1.0" "status=active" "launched_on=2025-01-15")
status_is "сервіс створено" 302 "$(httpcode "$r")"
SERVICE=$(location "$r" | grep -o '/services/[0-9]*' | grep -o '[0-9]\+')
[ -n "$SERVICE" ] && ok "id сервісу: $SERVICE" || fail "id сервісу не отримано"

r=$(post_form /t/service_authors "service_id=$SERVICE" "author_id=$AUTHOR" "role=lead")
status_is "авторство додано" 302 "$(httpcode "$r")"
r=$(post_form /t/service_versions "service_id=$SERVICE" "version=1.0" "released_on=2025-01-15" "changelog=Перший випуск")
status_is "версію додано" 302 "$(httpcode "$r")"
r=$(post_form /t/usage_terms "service_id=$SERVICE" "conditions=Лише для студентів факультету" \
  "rules=Не передавати доступ стороннім особам" "valid_until=2027-12-31")
status_is "умови використання додано" 302 "$(httpcode "$r")"
r=$(post_form /t/users "full_name=Смоук Користувач $SUFFIX" "email=user$SUFFIX@stud.example.edu" \
  "role=student" "group_name=ПІ-99" "registered_at=2025-02-01T09:30")
status_is "користувача зареєстровано" 302 "$(httpcode "$r")"
USER=$(find_id users "user$SUFFIX@stud")
r=$(post_form /t/registrations "user_id=$USER" "service_id=$SERVICE" "registered_at=2025-02-01T09:35" "status=active")
status_is "реєстрацію в сервісі додано" 302 "$(httpcode "$r")"
r=$(post_form /t/usage_stats "service_id=$SERVICE" "user_id=$USER" "used_on=2025-02-02" "sessions=3" "minutes=45")
status_is "статистику додано" 302 "$(httpcode "$r")"

echo "== Читання та пошук (SELECT)"
page=$(get "/services/$SERVICE")
contains "картка сервісу: назва" "Смоук-сервіс $SUFFIX" "$page"
contains "картка сервісу: автор" "Смоук Автор $SUFFIX" "$page"
contains "картка сервісу: умови" "Лише для студентів факультету" "$page"
contains "картка сервісу: реєстрація" "Смоук Користувач $SUFFIX" "$page"
found=$(curl -s -G --data-urlencode "q=Смоук-сервіс $SUFFIX" "$BASE_URL/services")
contains "пошук за назвою знаходить сервіс" "Смоук-сервіс $SUFFIX" "$found"
found=$(curl -s -G --data-urlencode "q=перевірки створення" "$BASE_URL/services")
contains "пошук за анотацією знаходить сервіс" "Смоук-сервіс $SUFFIX" "$found"
found=$(curl -s -G --data-urlencode "kind=$KIND" --data-urlencode "author=$AUTHOR" "$BASE_URL/services")
contains "пошук за видом і автором" "Смоук-сервіс $SUFFIX" "$found"
found=$(curl -s -G --data-urlencode "q=Смоук-сервіс $SUFFIX" --data-urlencode "status=deprecated" "$BASE_URL/services")
contains "фільтр за статусом відсікає сервіс" "Знайдено: 0" "$found"
found=$(curl -s -G --data-urlencode "q=Смоук-сервіс $SUFFIX" --data-urlencode "status=active" "$BASE_URL/services")
contains "фільтр за статусом залишає сервіс" "Знайдено: 1" "$found"
contains "статистика містить сервіс" "Смоук-сервіс $SUFFIX" "$(get /usage)"
contains "звіт за період містить сервіс" "Смоук-сервіс $SUFFIX" "$(get '/usage?from=2025-02-01&to=2025-02-28')"

echo "== Редагування (UPDATE)"
r=$(post_form "/t/services/$SERVICE" "name=Смоук-сервіс $SUFFIX" "annotation=Оновлена анотація $SUFFIX" \
  "kind_id=$KIND" "current_version=1.1.0" "status=deprecated" "launched_on=2025-01-15")
status_is "сервіс оновлено" 302 "$(httpcode "$r")"
page=$(get "/services/$SERVICE")
contains "нова версія видна" "1.1.0" "$page"
contains "нова анотація видна" "Оновлена анотація $SUFFIX" "$page"
contains "новий статус видно" "Застарілий" "$page"
found=$(curl -s -G --data-urlencode "q=Оновлена анотація $SUFFIX" "$BASE_URL/services")
contains "пошук бачить оновлений текст" "Смоук-сервіс $SUFFIX" "$found"

echo "== Перевірка коректності даних"
r=$(post_form "/t/services/$SERVICE" "name=Смоук-сервіс $SUFFIX" "annotation=x" "kind_id=$KIND" "current_version=abc" "status=active" "launched_on=2025-01-15")
status_is "некоректна версія відхилена" 422 "$(httpcode "$r")"
r=$(post_form /t/authors "full_name=Один" "email=not-an-email")
status_is "некоректний автор відхилений" 422 "$(httpcode "$r")"
r=$(post_form /t/usage_stats "service_id=$SERVICE" "user_id=$USER" "used_on=2025-02-02" "sessions=0" "minutes=10")
status_is "статистика без сеансів, але з хвилинами відхилена" 422 "$(httpcode "$r")"
r=$(post_form /t/users "full_name=Дублікат Пошти" "email=user$SUFFIX@stud.example.edu" "role=teacher" "registered_at=2025-02-01T09:30")
status_is "дублікат email відхилений базою" 409 "$(httpcode "$r")"
r=$(curl -s -i -X POST "$BASE_URL/t/service_kinds/$KIND/delete")
status_is "вид з сервісами видалити не можна (FK)" 409 "$(httpcode "$r")"

echo "== Перевірка в самій MySQL"
if command -v docker >/dev/null 2>&1 && docker ps --format '{{.Names}}' | grep -q '^faculty-services-db$'; then
  row=$(docker exec faculty-services-db mysql -ufaculty -pfaculty_pass faculty_services -N -B \
    -e "SELECT current_version, status FROM services WHERE id=$SERVICE" 2>/dev/null)
  if [ "$row" = "$(printf '1.1.0\tdeprecated')" ]; then ok "у БД: версія 1.1.0, статус deprecated"; else fail "у БД неочікуваний рядок: $row"; fi
else
  echo "  (пропущено: контейнер faculty-services-db не запущено)"
fi

echo "== Видалення (DELETE)"
r=$(curl -s -i -X POST "$BASE_URL/t/services/$SERVICE/delete")
status_is "сервіс видалено" 302 "$(httpcode "$r")"
status_is "картки сервісу вже немає" 404 "$(code "$BASE_URL/services/$SERVICE")"
# користувача й автора треба видаляти після сервісу (на них посилалась статистика, яка видалена каскадом)
for pair in "users/$USER" "authors/$AUTHOR" "service_kinds/$KIND"; do
  r=$(curl -s -i -X POST "$BASE_URL/t/$pair/delete")
  status_is "прибрано $pair" 302 "$(httpcode "$r")"
done

echo
echo "Пройдено: $PASS, провалено: $FAIL"
[ "$FAIL" -eq 0 ]
