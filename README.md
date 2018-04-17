# erlang_aaa

Erlang Authentication, Authorization, Accounting

Тестовое задание на стажировку

**Лицензия:** MIT

**Обратная связь:** bottleneko@gmail.com

**Текущая версия:** 0.1.0

## Сборка

    $ rebar3 compile


## Тестирование

    $ rebar3 ct

Тесты покрывают все приведенные в файле с заданием кейсы для проверок.

## Запуск

    $ rebar3 shell

## Ручное тестирование через curl

### Регистрация

    curl -H "Content-Type: application/json" 127.0.0.1:8080/user/registration/ -d '{"user":"login", "password":"password"}'

### Авторизация

    curl -H "Content-Type: application/json" 127.0.0.1:8080/user/auth/ -d '{"user":"login", "password":"password"}'
    >   {"token": "_%some token%_"}

### Смена пароля

    сurl -H "Content-Type: application/json" 127.0.0.1:8080/user/login -H "authorization: Bearer _%some token%_ " -d '{"old_password":"pass", "new_password":"pass1"}'

### Получение списка пользователей

    сurl -H "Content-Type: application/json" 127.0.0.1:8080/user -H "authorization: Bearer _%some token%_ " -d '{"old_password":"pass", "new_password":"pass1"}'

## Структура проекта

     Путь до файла                  | Содержимое
    --------------------------------|---------------------
    src/erlang_aaa_app.src          |   Точка входа в приложение
    src/erlang_aaa_app.erl          |   Точка входа в приложение с путями cowboy
    src/erlang_aaa_sup.erl          |   Cупервизор верхнего уровня
    src/registration_handler.erl    |   Обработчик запросов по /user/registration
    src/auth_handler.erl            |   Обработчик запросов по /user/auth
    src/change_password_handler.erl |   Обработчик запросов по /user/[:login]
    src/show_users_handler.erl      |   Обработчик запросов по /user/
    src/users_db.erl                |   Хранилище пользовательских аккаунтов и активных сессий
    src/users_utils.erl             |   Функции, которые использовались в нескольких модулях
    include/users.hrl               |   Заголовок с записями account и session

## Дополнительная информация

    * Регистрация пользователей с именами registration и auth не заблокирована, но смена пароля для них недоступна
    * Ошибки вида ```erlang {badmatch, Reason}``` отображаются для удобства отладки и тестирования. Let it crash.