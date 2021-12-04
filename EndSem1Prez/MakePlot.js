var elt = document.getElementById('calculator');
    var options = { border: false };
    var calculator = Desmos.GraphingCalculator(elt, options);
    calculator.setExpressions([
      {id:'1', latex:'x^2+y^2<25'}
    ]);