var gulp = require('gulp');
var sass = require('gulp-ruby-sass');
var autoprefixer = require('gulp-autoprefixer');
var uglify = require('gulp-uglify');
var minifier = require('gulp-minify-html');

gulp.task('sass', function(cb) {
	sass('scss', {precision: 8, style: 'compressed'}).on('err', function(err) {console.error('Error!', err);}).pipe(gulp.dest('mincss')).on('end', cb);
});

gulp.task('apf', ['sass'], function() {
	gulp.src('mincss/*.css').pipe(autoprefixer({browsers: ['last 2 versions'], cascade: false})).pipe(gulp.dest('mincss'));
});

gulp.task('minjs', function() {
	gulp.src('js/*.js').pipe(uglify()).pipe(gulp.dest('minjs'));
});

gulp.task('minhtml', function() {
	gulp.src('html/*.html').pipe(minifier()).pipe(gulp.dest('.'));
})

gulp.task('default', ['apf', 'minjs', 'minhtml']);
