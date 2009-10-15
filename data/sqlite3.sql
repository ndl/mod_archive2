CREATE TABLE archive_collection(id INTEGER NOT NULL,
                                prev_id INTEGER,
                                next_id INTEGER,
                                us VARCHAR(2047) NOT NULL,
                                with_user VARCHAR(1023),
                                with_server VARCHAR(1023) NOT NULL,
                                with_resource VARCHAR(1023),
                                utc DATETIME NOT NULL,
                                change_utc DATETIME,
                                version INTEGER,
                                deleted INTEGER,
                                subject VARCHAR(1023),
                                thread VARCHAR(1023),
                                crypt INTEGER,
                                extra VARCHAR(32767),
                                PRIMARY KEY(id));
CREATE INDEX IDX_archive_colls_prev_id ON archive_collection(prev_id);
CREATE INDEX IDX_archive_colls_next_id ON archive_collection(next_id);
CREATE INDEX IDX_archive_colls_us ON archive_collection(us);
CREATE INDEX IDX_archive_colls_with_server ON archive_collection(with_server);
CREATE INDEX IDX_archive_colls_with_user ON archive_collection(with_user);
CREATE INDEX IDX_archive_colls_with_resource ON archive_collection(with_resource);
CREATE INDEX IDX_archive_colls_utc ON archive_collection(utc);
CREATE INDEX IDX_archive_colls_change_utc ON archive_collection(change_utc);

CREATE TABLE archive_message(id INTEGER NOT NULL,
                             coll_id INTEGER NOT NULL,
                             utc DATETIME NOT NULL,
                             direction INTEGER,
                             body VARCHAR(65535),
                             name VARCHAR(1023),
                             jid VARCHAR(3071),
                             PRIMARY KEY(id));
CREATE INDEX IDX_archive_msgs_coll_id ON archive_message(coll_id);
CREATE INDEX IDX_archive_msgs_utc ON archive_message(utc);

CREATE TABLE archive_jid_prefs(us VARCHAR(2047) NOT NULL,
                               with_user VARCHAR(1023) NOT NULL,
                               with_server VARCHAR(1023) NOT NULL,
                               with_resource VARCHAR(1023) NOT NULL,
                               save INTEGER,
                               expire INTEGER,
                               otr INTEGER,
                               PRIMARY KEY(us, with_user, with_server, with_resource));

CREATE TABLE archive_global_prefs(us VARCHAR(2047) NOT NULL,
                                  save INTEGER,
                                  expire INTEGER,
                                  otr INTEGER,
                                  method_auto INTEGER,
                                  method_local INTEGER,
                                  method_manual INTEGER,
                                  auto_save INTEGER,
                                  PRIMARY KEY(us));

CREATE TRIGGER archive_collection_delete BEFORE DELETE ON archive_collection
FOR EACH ROW
BEGIN
  DELETE FROM archive_message WHERE coll_id = OLD.id;
  UPDATE archive_collection SET prev_id = null WHERE prev_id = OLD.id;
  UPDATE archive_collection SET next_id = null WHERE next_id = OLD.id;
END;

CREATE TRIGGER archive_collection_update BEFORE UPDATE ON archive_collection
FOR EACH ROW WHEN NEW.deleted = 1
BEGIN
  DELETE FROM archive_message WHERE coll_id = NEW.id;
  UPDATE archive_collection SET prev_id = null WHERE prev_id = NEW.id;
  UPDATE archive_collection SET next_id = null WHERE next_id = NEW.id;
END;
