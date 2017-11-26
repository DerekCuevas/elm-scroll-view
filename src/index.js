import './main.css';
import { Main } from './Main.elm';

const app = Main.embed(document.getElementById('root'));

app.ports.getBoundingClientRect.subscribe(({ id }) => {
  requestAnimationFrame(() => {
    const element = document.getElementById(id);

    if (element) {
      app.ports.setBoundingClientRect.send({
        id, rect: element.getBoundingClientRect()
      });
    }
  });
});
