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
