export function createPoll(poll) {
  const url = `${process.env.STRAW_POLL_API_URL}/polls`;

  return fetch(url, {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
    },
    body: JSON.stringify(poll),
  }).then((response) => {
    if (response.ok) {
      return response.json()
    } else {
      return Promise.reject(response)
    }
  });
}

export function getPoll(pollId) {
  const url = `${process.env.STRAW_POLL_API_URL}/polls/${encodeURI(pollId)}`;
  return fetch(url, {
    method: "GET",
    headers: {
      "Content-Type": "application/json",
    },
  }).then((response) => {
    if (response.ok) {
      return response.json()
    } else {
      return Promise.reject(response)
    }
  });
}

export function vote(pollId, answerId) {
  const url = `${process.env.STRAW_POLL_API_URL}/polls/${encodeURI(pollId)}/votes/${encodeURI(answerId)}`;
  return fetch(url, {
    method: "POST",
    headers: {
      "Content-Type": "application/json",
    },
  }).then((response) => {
    if (response.ok) {
      return response.json()
    } else {
      return Promise.reject(response)
    }
  });
}
