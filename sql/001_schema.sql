-- Інформаційні сервіси мережі факультету: схема бази даних (MySQL 8)
SET NAMES utf8mb4;

-- 1. Види сервісів (довідник)
CREATE TABLE service_kinds (
  id          BIGINT       NOT NULL AUTO_INCREMENT PRIMARY KEY,
  name        VARCHAR(100) NOT NULL UNIQUE,
  description VARCHAR(500) NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- 2. Автори (розробники та відповідальні)
CREATE TABLE authors (
  id         BIGINT       NOT NULL AUTO_INCREMENT PRIMARY KEY,
  full_name  VARCHAR(150) NOT NULL,
  email      VARCHAR(150) NOT NULL UNIQUE,
  position   VARCHAR(100) NULL,
  department VARCHAR(150) NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- 3. Сервіси: назва, анотація, вид, поточна версія
CREATE TABLE services (
  id              BIGINT       NOT NULL AUTO_INCREMENT PRIMARY KEY,
  name            VARCHAR(150) NOT NULL UNIQUE,
  annotation      TEXT         NOT NULL,
  kind_id         BIGINT       NOT NULL,
  current_version VARCHAR(30)  NOT NULL,
  status          ENUM('active','planned','deprecated') NOT NULL DEFAULT 'active',
  launched_on     DATE         NULL,
  CONSTRAINT fk_services_kind FOREIGN KEY (kind_id) REFERENCES service_kinds(id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- 4. Зв'язок "сервіс - автор" (багато-до-багатьох)
CREATE TABLE service_authors (
  id         BIGINT NOT NULL AUTO_INCREMENT PRIMARY KEY,
  service_id BIGINT NOT NULL,
  author_id  BIGINT NOT NULL,
  role       ENUM('lead','developer','maintainer') NOT NULL DEFAULT 'developer',
  UNIQUE KEY uq_service_author (service_id, author_id),
  CONSTRAINT fk_sa_service FOREIGN KEY (service_id) REFERENCES services(id) ON DELETE CASCADE,
  CONSTRAINT fk_sa_author  FOREIGN KEY (author_id)  REFERENCES authors(id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- 5. Історія версій сервісу
CREATE TABLE service_versions (
  id          BIGINT      NOT NULL AUTO_INCREMENT PRIMARY KEY,
  service_id  BIGINT      NOT NULL,
  version     VARCHAR(30) NOT NULL,
  released_on DATE        NOT NULL,
  changelog   TEXT        NULL,
  UNIQUE KEY uq_service_version (service_id, version),
  CONSTRAINT fk_sv_service FOREIGN KEY (service_id) REFERENCES services(id) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- 6. Умови, кінцевий термін та правила використання (по одному набору на сервіс)
CREATE TABLE usage_terms (
  id          BIGINT NOT NULL AUTO_INCREMENT PRIMARY KEY,
  service_id  BIGINT NOT NULL UNIQUE,
  conditions  TEXT   NOT NULL,
  rules       TEXT   NOT NULL,
  valid_until DATE   NULL,
  CONSTRAINT fk_ut_service FOREIGN KEY (service_id) REFERENCES services(id) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- 7. Користувачі (дані, які вводяться при реєстрації)
CREATE TABLE users (
  id            BIGINT       NOT NULL AUTO_INCREMENT PRIMARY KEY,
  full_name     VARCHAR(150) NOT NULL,
  email         VARCHAR(150) NOT NULL UNIQUE,
  role          ENUM('student','teacher','staff') NOT NULL DEFAULT 'student',
  group_name    VARCHAR(50)  NULL,
  registered_at DATETIME     NOT NULL
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- 8. Реєстрації користувачів у сервісах
CREATE TABLE registrations (
  id            BIGINT   NOT NULL AUTO_INCREMENT PRIMARY KEY,
  user_id       BIGINT   NOT NULL,
  service_id    BIGINT   NOT NULL,
  registered_at DATETIME NOT NULL,
  status        ENUM('active','blocked','cancelled') NOT NULL DEFAULT 'active',
  UNIQUE KEY uq_registration (user_id, service_id),
  CONSTRAINT fk_reg_user    FOREIGN KEY (user_id)    REFERENCES users(id),
  CONSTRAINT fk_reg_service FOREIGN KEY (service_id) REFERENCES services(id) ON DELETE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

-- 9. Статистика використання (за днями)
CREATE TABLE usage_stats (
  id         BIGINT NOT NULL AUTO_INCREMENT PRIMARY KEY,
  service_id BIGINT NOT NULL,
  user_id    BIGINT NOT NULL,
  used_on    DATE   NOT NULL,
  sessions   INT    NOT NULL,
  minutes    INT    NOT NULL,
  CONSTRAINT chk_stats_nonneg CHECK (sessions >= 0 AND minutes >= 0),
  CONSTRAINT fk_us_service FOREIGN KEY (service_id) REFERENCES services(id) ON DELETE CASCADE,
  CONSTRAINT fk_us_user    FOREIGN KEY (user_id)    REFERENCES users(id)
) ENGINE=InnoDB DEFAULT CHARSET=utf8mb4;

CREATE INDEX idx_stats_service_date ON usage_stats (service_id, used_on);
