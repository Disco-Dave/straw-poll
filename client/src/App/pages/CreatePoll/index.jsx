import React from "react";
import Control from "../../form/Control";
import DateInput from "../../form/DateInput";
import Answer from "./Answer";

const MINIMAL_ANSWERS = 2;

const initialField = () => ({
  value: "",
  error: "",
});

const initialState = () => ({
  question: initialField(),
  answers: Array.from({ length: MINIMAL_ANSWERS }).map(initialField),
  expiration: {
    value: null,
    error: "",
  },
});

const validate = {
  question(state) {
    const value = state?.question?.value?.trim?.();
    const error =
      value === null || value === undefined || value === ""
        ? "Question is required."
        : "";

    return {
      ...state,
      question: {
        ...state.question,
        error,
      },
    };
  },
  answer: (index) => (state) => {
    return {
      ...state,
      answers: state.answers.map((a, i) => {
        if (i === index) {
          const value = a?.value?.trim?.();
          const error =
            value === null || value === undefined || value === ""
              ? "Answer is required."
              : "";
          return {
            ...a,
            error,
          };
        } else {
          return a;
        }
      }),
    };
  },
  expiration(state) {
    const today = new Date();
    today.setHours(0);
    today.setMinutes(0);
    today.setSeconds(0);

    const error =
      state?.expiration?.value && state.expiration.value < today
        ? "Expiration may not be less than today."
        : "";
    return {
      ...state,
      expiration: {
        ...state.expiration,
        error,
      },
    };
  },
  all(state) {
    return this.question(
      this.expiration(
        state.answers.reduce((st, _, i) => this.answer(i)(st), { ...state })
      )
    );
  },
};

export default function CreatePoll() {
  const [state, setState] = React.useState(initialState());

  function handleOnSubmit(e) {
    e?.preventDefault?.();
    const validatedState = validate.all(state);
    setState(validatedState);

    if (
      !validatedState.expiration.error &&
      !validatedState.question.error &&
      validatedState.answers.every(({ error }) => !error)
    ) {
      fetch(`${process.env.API_URL}/polls`, {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
        },
        body: JSON.stringify({
          question: validatedState.question.value,
          expiration: validatedState.expiration.value,
          answers: validatedState.answers.map(({ value }) => value),
        }),
      });
    }
  }

  return (
    <main className="container">
      <h1>Create Poll</h1>

      <form
        className="form"
        onSubmit={handleOnSubmit}
        onReset={() => setState(initialState())}
      >
        <Control
          htmlFor="question"
          label="Question"
          error={state.question.error}
        >
          <input
            id="question"
            name="question"
            type="text"
            className="control__input"
            value={state.question.value}
            onChange={(e) =>
              setState({
                ...state,
                question: {
                  ...state.question,
                  value: e.target.value,
                },
              })
            }
            onBlur={() => setState(validate.question)}
          />
        </Control>

        <Control
          htmlFor="expiration"
          label="Expiration"
          error={state.expiration.error}
          info="optional"
        >
          <DateInput
            id="question"
            name="question"
            className="control__input"
            value={state.expiration.value}
            min={new Date()}
            onChange={(e) =>
              setState({
                ...state,
                expiration: {
                  ...state.expiration,
                  value: e.target.value,
                },
              })
            }
            onBlur={() => setState(validate.expiration)}
          />
        </Control>

        {state.answers.map((answer, index) => (
          <Answer
            key={index}
            answerNumber={index + 1}
            value={answer.value}
            error={answer.error}
            showRemove={state.answers.length > MINIMAL_ANSWERS}
            onRemove={() =>
              setState({
                ...state,
                answers: state.answers.filter((_, i) => i !== index),
              })
            }
            onChange={(e) =>
              setState({
                ...state,
                answers: state.answers.map((v, i) =>
                  i === index ? { ...v, value: e.target.value } : v
                ),
              })
            }
            onBlur={() => setState(validate.answer(index))}
          />
        ))}

        <div className="control-link">
          <button
            className="button button--link"
            type="button"
            onClick={() =>
              setState({
                ...state,
                answers: [...state.answers, initialField()],
              })
            }
          >
            Add answer
          </button>
        </div>

        <div className="action-buttons">
          <button className="button button--primary" type="submit">
            Create
          </button>
          <button className="button button--secondary" type="reset">
            Reset
          </button>
        </div>
      </form>
    </main>
  );
}
