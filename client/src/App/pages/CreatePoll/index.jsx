import React from "react";
import Control from "../../form/Control";
import Answer from "./Answer";
import Expiration from "./Expiration";

const MINIMAL_ANSWERS = 2;

const initialField = () => ({
  value: "",
  error: "",
});

const initialState = () => ({
  question: initialField(),
  answers: Array.from({ length: MINIMAL_ANSWERS }).map(initialField),
});

export default function CreatePoll() {
  const [state, setState] = React.useState(initialState());

  function handleOnChange(e) {
    const recognizedFields = Object.keys(state);

    if (e?.target?.name && recognizedFields.includes(e.target.name)) {
      setState({
        ...state,
        [e.target.name]: e.target.value,
      });
    }
  }

  function handleOnSubmit(e) {
    e?.preventDefault?.();
  }

  function handleOnReset() {
    setState(initialState());
  }

  function handleAddAnswer(e) {
    e?.preventDefault?.();
    setState({
      ...state,
      answers: [...state.answers, initialField()],
    });
  }

  const handleRemoveAnswer = (index) => (e) => {
    e?.preventDefault?.();
    setState({
      ...state,
      answers: state.answers.filter((_, i) => i !== index),
    });
  };

  const handleAnswerChange = (index) => (e) => {
    setState({
      ...state,
      answers: state.answers.map((v, i) =>
        i === index ? { ...v, value: e.target.value } : v
      ),
    });
  };

  const handleAnswerBlur = (index) => () => {
    console.log("Blurred ", index);
  };

  return (
    <main className="container">
      <h1>Create Poll</h1>

      <form className="form" onSubmit={handleOnSubmit} onReset={handleOnReset}>
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
            onChange={handleOnChange}
          />
        </Control>

        <Expiration />

        {state.answers.map((answer, index) => (
          <Answer
            key={index}
            answerNumber={index + 1}
            value={answer.value}
            error={answer.error}
            showRemove={state.answers.length > MINIMAL_ANSWERS}
            onRemove={handleRemoveAnswer(index)}
            onChange={handleAnswerChange(index)}
            onBlur={handleAnswerBlur(index)}
          />
        ))}

        <div className="control-link">
          <button className="button button--link" onClick={handleAddAnswer}>
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
