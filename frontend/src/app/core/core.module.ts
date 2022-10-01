import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { AppLayoutComponent } from './layouts/app-layout/app-layout.component';
import { RouterModule } from '@angular/router';
import { HomePageComponent } from './pages/home-page/home-page.component';

@NgModule({
  declarations: [AppLayoutComponent, HomePageComponent],
  imports: [CommonModule, RouterModule],
})
export class CoreModule {}
