-define(ARCHIVE_COLLECTION1,
        #archive_collection{
            us = ?JID,
            with_user = "juliet",
            with_server = "capulet.com",
            with_resource = "chamber",
            utc = {{1469, 07, 21}, {02, 56, 15}},
            change_utc = {{2002, 12, 31}, {23, 59, 59}},
            version = 1,
            deleted = false}).

-define(ARCHIVE_COLLECTION2,
        #archive_collection{
            us = ?JID,
            with_user = "balcony",
            with_server = "house.capulet.com",
            utc = {{1469, 07, 21}, {03, 16, 37}},
            change_utc = {{2001, 12, 31}, {23, 59, 59}},
            version = 1,
            deleted = false}).

-define(ARCHIVE_COLLECTION3,
        #archive_collection{
            us = ?JID,
            with_user = "benvolio",
            with_server = "montague.net",
            utc = {{1469, 07, 21}, {03, 01, 54}},
            change_utc = {{2000, 12, 31}, {23, 59, 59}},
            version = 1,
            deleted = false}).