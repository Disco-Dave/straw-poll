CREATE TABLE public.answers (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    poll_id BIGINT NOT NULL REFERENCES public.polls(id),
    answer TEXT NOT NULL,
    created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP
);
