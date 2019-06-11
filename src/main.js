window.onload = () => {
	const dbFields = "dbFields"
	const storedDbFieldsValue = localStorage.getItem(dbFields);

	const app = Elm.Main.init({flags: JSON.parse(storedDbFieldsValue)})

	app.ports.saveDbFields.subscribe((state) => {
  	localStorage.setItem(dbFields, JSON.stringify(state))
	});

	app.ports.clearDbFields.subscribe(_ => {
		localStorage.removeItem(dbFields);
	});
}
