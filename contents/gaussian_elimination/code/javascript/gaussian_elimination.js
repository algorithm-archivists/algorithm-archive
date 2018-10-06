function gaussian_elimination(a){
    var rows = a.length
    var cols = a[0].length
    var row = 0;

    for (let col = 0; col < cols - 1; ++col) {

        let pivot = row;
        for (let i = row + 1; i < rows; ++i) {
            if (Math.abs(a[i][col]) > Math.abs(a[pivot][col])) {
                pivot = i;
            }
        }

        if (a[pivot][col] == 0) {
            console.log("The matrix is singular.\n");
            continue;
        }

        if (col != pivot) {
            let t=a[col];
            a[col]=a[pivot];
            a[pivot]=t;
        }

        for (let i = row + 1; i < rows; ++i) {
            let scale = a[i][col] / a[row][col];

            for (let j = col + 1; j < cols; ++j) {
                a[i][j] -= a[row][j] * scale;
            }

            a[i][col] = 0;
        }

        ++row;
    }
    return a;
}

function back_substitution(a){
    var rows = a.length;
    var cols = a[0].length;
    var sol=new Array(rows);

    for (let i = rows - 1; i >= 0; --i) {

        let sum = 0;
        for (let j = cols - 2; j > i; --j) {
            sum += sol[j] * a[i][j];
        }

        sol[i] = (a[i][cols - 1] - sum) / a[i][i];
    }
    return sol;
}

function gauss_jordan(a) {
    var rows = a.length;
    var cols = a[0].length;
    var row = 0;

    for (let col = 0; col < cols - 1; ++col) {
        if (a[row][col] != 0) {
            for (let i = cols - 1; i > col - 1; --i) {
                a[row][i] /= a[row][col];
            }

            for (let i = 0; i < row; ++i) {
                for (let j = cols - 1; j > col - 1; --j) {
                    a[i][j] -= a[i][col] * a[row][j];
                }
            }

            ++row;
        }
    }
}

var a = [[3, 2 , -4, 3 ],
	      [ 2, 3 , 3 , 15],
	      [ 5, -3, 1 , 14]];

gaussian_elimination(a);
console.log("Gaussian elimination:\n");
for(let i=0;i<a.length;++i){
    let txt=""
    for(let j=0;j<a[i].length;++j){
        txt+=a[i][j]<0?" ":"  ";
        txt+=a[i][j].toPrecision(8);
    }
    console.log(txt);
}

gauss_jordan(a);
console.log("\nGauss-Jordan:\n");
for(let i=0;i<a.length;++i){
    let txt=""
    for(let j=0;j<a[i].length;++j){
        txt+=a[i][j]<0?" ":"  ";
        txt+=a[i][j].toPrecision(8);
    }
    console.log(txt);
}

var sol=back_substitution(a),txt="";
console.log("\nSolutions are:\n");
for(let i=0;i<sol.length;++i){
    txt+=sol[i]<0?" ":"  ";
    txt+=sol[i].toPrecision(8);
}
console.log(txt)
