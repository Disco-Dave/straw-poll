import React from "react";
import Control from "../../form/Control";
import Answer from "./Answer";

export default function CreatePoll() {
  const initialField = () => ({
    value: "",
    error: "",
  });

  const initialState = () => ({
    question: initialField(),
    answers: [initialField(), initialField()],
  });

  const [state, setState] = React.useState(initialState());

  const answerKeys = Object.keys(state).filter((k) => k.startsWith("answer"));
  answerKeys.sort();

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
    console.log('Blurred ', index);
  }

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

        {state.answers.map((answer, index) => (
          <Answer
            key={index}
            answerNumber={index + 1}
            value={answer.value}
            error={answer.error}
            showRemove={state.answers.length > 2}
            onRemove={handleRemoveAnswer(index)}
            onChange={handleAnswerChange(index)}
            onBlur={handleAnswerBlur(index)}
          />
        ))}

        <div className="control-link">
          <a href="#" className="link-button" onClick={handleAddAnswer}>
            Add another answer
          </a>
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
