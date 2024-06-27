//---------------------------------------------------------------------
//  Name:       HelloSDL2.cpp
//  Author:     EAML
//  Date:       2021-05-16
// 
//  Description:    
//      A minimal PoC for producing a native SDL2 Windows app that can
//      be ran from either Windows Explorer or from Powershell console.
//      It's designed to use minimal command line, compiler options, 
//      and dependencies... It will display a gray window for 2 sec's.
//
//  Dependencies:
//      [1] LLVM Clang++ compiler package
//      [2] SDL2 Libraries (DLL's) and Header files (*.h)
//      [3] TTF Libraries (DLL's) and Header files (*.h)
// 
//  Notes: 
//      There is a slight variation in the bahaviour, depending on: 
//      (a) if you compile as a Windows GUI:  the text will not show.
//      (b) if you compile as a console CLI:  text will show in both terminal and/or in a 2nd new window
//      (c) You may need to use "main()" for console and "WinMain()" for GUI...
//      (c) to install on Linux, use packages:  clang, libsdl2-dev
//      (d) Someone said: #define SDL_MAIN_HANDLED ...
//
//  To Run: 
//      cp .\SDL2\lib\x64\SDL2.dll C:\Windows\.     # For SDL2
//      cp .\SDL2_ttf\lib\x64\*.dll C:\Windows\.    # For SDL2 TTF
//      cp C:\Windows\Fonts\arial.ttf .             # Get a font...
// 
//  For a CLI version, with console output in 2nd Window:
//  # clang++.exe -std=c++17 main.cpp -o main.exe -L .\SDL2\lib\x64\ -L .\SDL2_ttf\lib\x64\ -I .\SDL2_ttf\include\ -I .\SDL2\include\ -lShell32 -lSDL2main -lSDL2 -lSDL2_ttf -Wno-narrowing -Xlinker /subsystem:console
//
//  For a GUI version, without any console output:
//  # clang++.exe -std=c++17 main.cpp -o main.exe -L .\SDL2\lib\x64\ -L .\SDL2_ttf\lib\x64\ -I .\SDL2_ttf\include\ -I .\SDL2\include\ -lShell32 -lSDL2main -lSDL2 -lSDL2_ttf -Wno-narrowing -Xlinker /subsystem:windows
// 
//  References:
//      [1] https://github.com/llvm/llvm-project/releases
//      [2] http://www.libsdl.org/release/SDL2-devel-2.0.14-VC.zip
//      [3] https://www.libsdl.org/projects/SDL_ttf/release/SDL2_ttf-devel-2.0.15-VC.zip
//      [4] https://www.libsdl.org/projects/SDL_ttf/docs/SDL_ttf.html
//      [5] http://www.sdltutorials.com/sdl-ttf
//      [6] https://www.libsdl.org/release/SDL-1.2.15/docs/html/sdlevent.html
//      [7] https://stackoverflow.com/q/67559556/
//---------------------------------------------------------------------
#include <SDL2/SDL.h>
#include <SDL2/SDL_ttf.h>
/* #include "SDL2/include/SDL.h" */
/* #include "SDL2_ttf/include/SDL_ttf.h" */

#include <stdio.h>
#include <cstdio>
#include <string>
#include <sstream>

#define SCREEN_WIDTH    640
#define SCREEN_HEIGHT   480
#define WINDOW_TITLE    "Hello SDL2!"
#define WINDOW_TEXT     "Hello World!"


// We make a custom stream handler for brievity
inline std::ostream& operator<<(std::ostream& os, SDL_version const& v) {
    os << int(v.major) << '.' << int(v.minor) << '.' << int(v.patch);
    return os;
}

std::string getInfo() {
    SDL_version aa, bb, cc;
    SDL_VERSION(&aa);
    SDL_GetVersion(&bb);
    SDL_TTF_VERSION(&cc);

    std::ostringstream oss;
    oss << "SDL version  : " << aa << '\n';
    oss << "SDL linker   : " << bb << '\n';
    oss << "SDL_TTF ver. : " << cc << '\n';
    return oss.str();
}

void drawText ( SDL_Surface* screen, char* string, int size, int x, int y, SDL_Color fgC, SDL_Color bgC) {
    // Remember to call TTF_Init(), TTF_Quit(), before/after using this function.
    TTF_Font* font = TTF_OpenFont("arial.ttf", size);
    if(!font) {
        printf("[ERROR] TTF_OpenFont() Failed with: %s\n", TTF_GetError());
        exit(2);
    }
    TTF_SetFontStyle(font, TTF_STYLE_BOLD);
    //SDL_Surface* textSurface = TTF_RenderText_Solid(font, string, fgC);     // aliased glyphs
    SDL_Surface* textSurface = TTF_RenderText_Shaded(font, string, fgC, bgC);   // anti-aliased glyphs
    SDL_Rect textLocation = { x, y, 0, 0 };
    SDL_BlitSurface(textSurface, NULL, screen, &textLocation);
    SDL_FreeSurface(textSurface);
    TTF_CloseFont(font);
    //printf("[ERROR] Unknown error in drawText(): %s\n", TTF_GetError()); return 1;
}

//---------------------------------------------------------------------
//  MAIN
//---------------------------------------------------------------------
int main(int argc, char* args[]) {
    
    SDL_Window* window = NULL;                      // The window we are rendering to
    SDL_Surface* screenSurface = NULL;              // The surface contained by the window
    SDL_Event wEvent;                               // Enable the Window Event handler...
  
    if (SDL_Init(SDL_INIT_VIDEO) < 0) {
        printf( "[ERROR] SDL could not initialize! SDL Error: %s\n", SDL_GetError());
        return 1;
    }
    
    window = SDL_CreateWindow(WINDOW_TITLE, SDL_WINDOWPOS_UNDEFINED, SDL_WINDOWPOS_UNDEFINED,SCREEN_WIDTH, SCREEN_HEIGHT, SDL_WINDOW_SHOWN);
    if (window == NULL) {
        printf( "[ERROR] Window could not be created! SDL Error: %s\n", SDL_GetError());
        return 1;
    }
    
    screenSurface = SDL_GetWindowSurface(window);
    SDL_FillRect(screenSurface, NULL, SDL_MapRGB(screenSurface->format, 0x80, 0x80, 0x80));     // Set a gray background canvas
    SDL_UpdateWindowSurface(window);

    //-----------------------------------------------------
    // Draw the Text
    //-----------------------------------------------------
    if(TTF_Init() == -1) {
        printf("[ERROR] TTF_Init() Failed with: %s\n", TTF_GetError());
        exit(2);
    }
    
    SDL_Color fgC1 = { 0xff,0xff,0xff }, bgC1 = {0x00,0x00,0xa0};                               // white text on blue background
    SDL_Color fgC2 = { 0x00,0x00,0x00 }, bgC2 = {0xff,0x00,0xff};                               // black text on magenta background
    drawText( screenSurface, (char*) "Hello World! @ (x=50, y=100)", 18,  50,100, fgC1, bgC1);  // 18 pt @ (x=50,y=100)
    drawText( screenSurface, (char*) "arial.ttf @ (x=200, y=150)",   16, 200,150, fgC2, bgC2);  // 16 pt @ (x=200,y=150)

    int i=0;
    const int FSIZE = 12;               // Font Size
    std::string ver = getInfo();
    const char *pStr = ver.c_str();     // Convert string to char array
    
    //-----------------------------------------------------
    // Split on '\n' and draw each line further down
    //-----------------------------------------------------
    char *tok = strtok( (char*) pStr, "\n");
    while (tok != NULL) {
        drawText( screenSurface, (char*) tok, FSIZE, 50, (200 + (FSIZE + 4)*i), fgC1, fgC2);
        tok = strtok(NULL,"\n");
        i++;
    }

    SDL_UpdateWindowSurface(window);
    TTF_Quit();
    
    //-----------------------------------------------------
    // Wait for Events to quit & close the window
    //-----------------------------------------------------
    SDL_Delay(1000);  // Wait 1 sec for greasy fingers
    bool eQuit = false;
    while (!eQuit) {
        while(SDL_PollEvent(&wEvent)) {
            switch (wEvent.type) {
                case SDL_QUIT:              eQuit = true; break;
                case SDL_KEYDOWN:           eQuit = true; break;
                case SDL_MOUSEBUTTONDOWN:   eQuit = true; break;
                case SDL_WINDOWEVENT_CLOSE: eQuit = true; break;
            default:
                //SDL_Log("Window %d got unknown event %d\n", wEvent.window.windowID, wEvent.window.event);
                break;
            }
        }
        SDL_Delay(100); // Keep < 500 [ms]
    }

    SDL_DestroyWindow(window);
    SDL_Quit();
    return 0;
}
