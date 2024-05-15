const fs = require('node:fs/promises');

const generateDebugLabels = async () => {
  try {
    const labels = (await fs.readFile('cart.lbl', { encoding: 'utf8' }))
      .split('\n')
      .reverse()
      .filter((line) => (line !== ''))                                    // remove blank label
      .map((d) => {                                                       // format labels
        let [, address, text] = d.split(' ');
        address = '$' + address.slice(1);
        text = text.slice(1);
        return `${address}#${text}#`;
      })
      .reduce((prev, curr) => {                                           // concat all labls to one string
        return prev + '\n' + curr;
      });

    await fs.writeFile('cart.nes.0.nl', labels);
  }
  catch (err) {
    console.log(err);
  };
};

generateDebugLabels();