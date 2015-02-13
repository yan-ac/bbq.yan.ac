module.exports = function (grunt) {

  grunt.initConfig({
    pkg: grunt.file.readJSON('package.json'),
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

  grunt.loadNpmTasks('grunt-contrib-sass');
  grunt.registerTask('default', ['sass']);
};
