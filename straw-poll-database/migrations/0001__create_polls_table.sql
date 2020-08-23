CREATE TABLE public.polls (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    question TEXT NOT NULL,
    expiration TIMESTAMP WITH TIME ZONE NULL,
    created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP
);
