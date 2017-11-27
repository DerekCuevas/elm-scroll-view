import './main.css';
import { Main } from './Main.elm';

const app = Main.embed(document.getElementById('root'));

function withElementIfExists(id, fn) {
  requestAnimationFrame(() => {
    const element = document.getElementById(id);
    if (element) {
      fn(element);
    }
  });
}

app.ports.getBoundingClientRect.subscribe(({ id }) => {
  withElementIfExists(id, (element) => {
    app.ports.setBoundingClientRect.send({
      id, rect: element.getBoundingClientRect()
    });
  });
});

app.ports.getScrollWidth.subscribe(({ id }) => {
  withElementIfExists(id, (element) => {
    console.log(element.scrollWidth);
    app.ports.setScrollWidth.send({
      id, scrollWidth: element.scrollWidth
    });
  });
});
