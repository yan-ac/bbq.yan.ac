module.exports = function (grunt) {

  grunt.initConfig({
    pkg: grunt.file.readJSON('package.json'),
    traceur: {
      custom: {
        files: [{
          expand: true,
          cwd: 'views/js-es6',
          src: ['*.js'],
          dest: 'views/js'
        }]
      }
    },
    sass: {
      dist: {
        files: [{
          expand: true,
          cwd: 'views/sass',
          src: ['*.scss'],
          dest: 'views/css',
          ext: '.css'
        }]
      }
    }
  });

  grunt.loadNpmTasks('grunt-traceur');
  grunt.loadNpmTasks('grunt-contrib-sass');
  grunt.loadNpmTasks('grunt-newer');

  grunt.registerTask('default', ['newer:traceur'], ['sass']);
};
