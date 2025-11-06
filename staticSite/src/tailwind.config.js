module.exports = {
  purge: {
    enabled: true,
    content: ['./html/*.html',
	      './html/**/*.html'
             ]
  },
  theme: {
    extend: {
      colors: {
        primary: "#14a9db",
        secondary: "#2a89dc",
      },
    },
  },
}
