CREATE DATABASE IF NOT EXISTS ejabberd CHARACTER SET utf8 COLLATE utf8_general_ci;

USE ejabberd;

SET table_type=InnoDB;

CREATE TABLE archive_collection(id INTEGER NOT NULL AUTO_INCREMENT,
                                prev_id INTEGER,
                                next_id INTEGER,
                                us VARCHAR(2047) NOT NULL,
                                with_user VARCHAR(1023),
                                with_server VARCHAR(1023) NOT NULL,
                                with_resource VARCHAR(1023),
                                utc DATETIME NOT NULL,
                                change_utc DATETIME,
                                version INTEGER,
                                deleted TINYINT,
                                subject VARCHAR(1023),
                                thread VARCHAR(1023),
                                crypt TINYINT,
                                extra VARCHAR(32767),
                                PRIMARY KEY(id))
                                CHARACTER SET utf8
                                COLLATE utf8_general_ci;
CREATE INDEX IDX_archive_colls_prev_id ON archive_collection(prev_id);
CREATE INDEX IDX_archive_colls_next_id ON archive_collection(next_id);
CREATE INDEX IDX_archive_colls_us ON archive_collection(us(16));
CREATE INDEX IDX_archive_colls_with_server ON archive_collection(with_server(8));
CREATE INDEX IDX_archive_colls_with_user ON archive_collection(with_user(8));
CREATE INDEX IDX_archive_colls_with_resource ON archive_collection(with_resource(8));
CREATE INDEX IDX_archive_colls_utc ON archive_collection(utc);
CREATE INDEX IDX_archive_colls_change_utc ON archive_collection(change_utc);

CREATE TABLE archive_message(id INTEGER NOT NULL AUTO_INCREMENT,
                             coll_id INTEGER NOT NULL,
                             utc DATETIME NOT NULL,
                             direction TINYINT,
                             body VARCHAR(63488),
                             name VARCHAR(1023),
                             jid VARCHAR(3071),
                             PRIMARY KEY(id))
                             CHARACTER SET utf8
                             COLLATE utf8_general_ci;
CREATE INDEX IDX_archive_msgs_coll_id ON archive_message(coll_id);
CREATE INDEX IDX_archive_msgs_utc ON archive_message(utc);

CREATE TABLE archive_jid_prefs(us VARCHAR(2047) NOT NULL,
                               with_user VARCHAR(1023),
                               with_server VARCHAR(1023) NOT NULL,
                               with_resource VARCHAR(1023),
                               exactmatch TINYINT,
                               save TINYINT,
                               expire INTEGER,
                               otr TINYINT)
                               CHARACTER SET utf8
                               COLLATE utf8_general_ci;
CREATE UNIQUE INDEX IDX_archive_jid_prefs_key ON archive_jid_prefs(us(16), with_user(8), with_server(8), with_resource(8), exactmatch);

CREATE TABLE archive_global_prefs(us VARCHAR(2047) NOT NULL,
                                  save TINYINT,
                                  expire INTEGER,
                                  otr TINYINT,
                                  method_auto TINYINT,
                                  method_local TINYINT,
                                  method_manual TINYINT,
                                  auto_save TINYINT,
                                  PRIMARY KEY  (us(16)))
                                  CHARACTER SET utf8
                                  COLLATE utf8_general_ci;

DELIMITER |

CREATE TRIGGER archive_collection_delete BEFORE DELETE ON archive_collection
FOR EACH ROW
BEGIN
  DELETE FROM archive_message WHERE coll_id = OLD.id;
END;
|

CREATE TRIGGER archive_collection_update BEFORE UPDATE ON archive_collection
FOR EACH ROW
BEGIN
  IF NEW.deleted = 1 THEN
    DELETE FROM archive_message WHERE coll_id = NEW.id;
  END IF;
END;
|

DELIMITER ;
