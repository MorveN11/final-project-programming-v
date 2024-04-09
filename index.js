import express from 'express';
import path from 'path';

const app = express();
const port = process.env.PORT;

app.use(express.static(path.join(process.cwd(), './result/bin/final-project-programming-v-exe.jsexe')));

app.use((_, res) => {
  res.status(404);
  res.send('<h1>404 Not Found</h1>');
});

app.listen(port, () => {
  console.log(`App listening on port ${port}`);
});
