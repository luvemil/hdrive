CREATE TABLE stores (
    -- id SERIAL PRIMARY KEY,
    store_name VARCHAR(256) PRIMARY KEY, -- UNIQUE NOT NULL,
    bucket TEXT NOT NULL,
    "path" TEXT NOT NULL
);

CREATE TABLE dir_nodes (
    title TEXT NOT NULL,
    full_path TEXT NOT NULL,
    thumb TEXT,
    uuid VARCHAR(256) PRIMARY KEY,
    dir_id VARCHAR(256),
    -- store_id SERIAL NOT NULL,
    store_name VARCHAR(256) NOT NULL,
    CONSTRAINT fk_parent_dir
        FOREIGN KEY (dir_id)
            REFERENCES dir_nodes(uuid),
    -- CONSTRAINT fk_dirstore
    --     FOREIGN KEY (store_id)
    --         REFERENCES stores(id)
    CONSTRAINT fk_dirstore
        FOREIGN KEY (store_name)
            REFERENCES stores(store_name)
);

CREATE TABLE file_nodes (
    title TEXT NOT NULL,
    file_type VARCHAR(10) NOT NULL, -- enum actually
    size INT,
    uuid VARCHAR(256) PRIMARY KEY,
    "path" TEXT NOT NULL,
    thumb TEXT,
    dir_id VARCHAR(256),
    CONSTRAINT fk_filedir
        FOREIGN KEY (dir_id)
            REFERENCES dir_nodes(uuid)
);
