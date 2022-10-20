import { NgModule } from '@angular/core';
import { BrowserModule } from '@angular/platform-browser';
import { RouterModule } from '@angular/router';

import { AppRoutingModule } from './app-routing.module';
import { AppComponent } from './app.component';
import { CoreModule } from './core/core.module';
import { MatchPageComponent } from './core/pages/match-page/match-page.component';
import { SwipePageComponent } from './core/pages/swipe-page/swipe-page.component';

@NgModule({
  declarations: [AppComponent, MatchPageComponent, SwipePageComponent],
  imports: [BrowserModule, AppRoutingModule, RouterModule, CoreModule],
  providers: [],
  bootstrap: [AppComponent],
})
export class AppModule {}
