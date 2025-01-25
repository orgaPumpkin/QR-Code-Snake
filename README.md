![Snake](https://github.com/orgaPumpkin/QR-Code-Snake/blob/main/snake.png?raw=true)
# A snake game for Windows, written entirely in Assembly.
This snake game is written in x86 asm, only using the WinAPI to make it as small as possible.

## Run It Yourself
Running the game is as simple as downloading the [snake.exe](https://github.com/orgaPumpkin/QR-Code-Snake/blob/main/snake.exe) file and running it.

Note: Windows Defender might detect the exe as a virus, so you'll need to open the protection history and click revert.

## A scannable QR Code that stores the entire game:
Scan the code, rename the downloaded file to an .exe file and run it! (you might need to copy and paste the url into the browser manualy.)
![QR Code](https://github.com/orgaPumpkin/QR-Code-Snake/blob/main/QRCode.png?raw=true)

## Build It Yourself
First thing, clone the repository to your machine:

    git clone https://github.com/orgaPumpkin/QR-Code-Snake.git

Then, open the `Developer Command Prompt for VS` from the start menu (requires VS installed), and navigate to the repo's folder.

Now, there are two ways of building the code.

1. Normal build using masm:

        build.bat

2. Using the Crinkler to build the smaller version
(Download the Crinkler from [here](https://github.com/runestubbe/Crinkler/releases/tag/v2.3),
and extruct the x32 version to the repo's folder as `crinkler.exe`). 

   Then use the following command to build it into a tiny executable:

        crinkle.bat

## How It Was Made
When I started this project, I had no idea how to make a game in Assembly. I had to learn how to use the WinAPI to
 draw to the screen and to handle input. It was a lot of fun, and I learned a lot.
