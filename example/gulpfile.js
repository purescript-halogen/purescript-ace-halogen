
/* jshint node: true */
"use strict";

var gulp = require("gulp");
var purescript = require("gulp-purescript");
var webpack = require("webpack-stream");

var sources = [
  "src/**/*.purs",
  "bower_components/purescript-*/src/**/*.purs"
];

var foreigns = [
  "src/**/*.js",
  "bower_components/purescript-*/src/**/*.js"
];

var ourSources =
    [sources[0], foreigns[0]]

gulp.task("make", function() {
  return purescript.psc({ src: sources, ffi: foreigns });
});

gulp.task("prebundle", ["make"], function() {
  return purescript.pscBundle({
    src: "output/**/*.js",
    output: "dist/main.js",
    module: "Main",
    main: "Main"
  });
});

// Watch Files For Changes
gulp.task('type-watch', function() {
  gulp.watch(ourSources, ['make']);
});

gulp.task('watch', function() {
  gulp.watch(ourSources, ['bundle']);
});

gulp.task("bundle", ["prebundle"], function () {
  return gulp.src("dist/main.js")
    .pipe(webpack({
      resolve: { modulesDirectories: ["node_modules"] },
      output: { filename: "main.js" }
    }))
    .pipe(gulp.dest("dist"));
});

gulp.task("psci", function() {
    return purescript.psci({ src: sources, ffi: foreigns })
        .pipe(gulp.dest("."));;
});

gulp.task("default", ["bundle"]);
