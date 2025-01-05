namespace FIOChat.Client.TerminalGUI

open FIO.Core
open Terminal.Gui

type ClientApp() =
    inherit FIOApp<unit, string>()

    override this.effect = fio {
        // Initialize the Application
        Application.Init()

        // Create a top-level window
        let top = Application.Top

        // Create the main window (similar to a "Form" in other GUI frameworks)
        let mainWin = new Window("FIOChat")
        top.Add(mainWin)

        let serverLabel = new Label(
            Text = "Server: ",
            X = Pos.Center() - Pos.Percent(15F),
            Y = Pos.Center()
        )

        let serverText = new TextField(
            X = Pos.Right serverLabel + Pos.Percent(1F),
            Y = Pos.Y serverLabel,
            Width = Dim.Percent(30F)
        )

        let usernameLabel = new Label(
            Text = "Username: ",
            X = Pos.Left serverLabel,
            Y = Pos.Bottom serverLabel + Pos.Percent(5F)
        )

        let usernameText = new TextField(
            X = Pos.Left serverText,
            Y = Pos.Top usernameLabel,
            Width = Dim.Percent(30F)
        )

        let button = new Button(
            Text = "Join FIOChat",
            X = Pos.Center(),
            Y = Pos.Bottom usernameLabel + Pos.Percent(5F),
            IsDefault = true
        )

        // Button click event
        button.add_Clicked(fun s ->
            let server = serverText.Text.ToString()
            let username = usernameText.Text.ToString()

            if (username = "admin") then
                MessageBox.Query("Logging in", "Login Successful", "Ok") |> ignore
                ()
            else
                MessageBox.ErrorQuery ("Logging In", "Incorrect username or password", "Ok") |> ignore
                ()
        )
    
        mainWin.Add(serverLabel, serverText, usernameLabel, usernameText, button)

        // Run the application
        Application.Run()

        // Clean up after application ends
        Application.Shutdown()
    }
