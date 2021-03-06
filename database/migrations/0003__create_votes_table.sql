CREATE TABLE public.votes (
    id BIGSERIAL NOT NULL PRIMARY KEY,
    answer_id BIGINT NOT NULL REFERENCES public.answers(id),
    poll_id BIGINT NOT NULL REFERENCES public.polls(id),
    created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP
);
