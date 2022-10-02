import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { AppLayoutComponent } from './layouts/app-layout/app-layout.component';
import { RouterModule } from '@angular/router';
import { HomePageComponent } from './pages/home-page/home-page.component';
import { DashboardComponent } from './pages/dashboard/dashboard.component';
import { CoreRoutingModule } from './core-routing.module';
import { SimpleAppLayoutComponent } from './layouts/simple-app-layout/simple-app-layout.component';

@NgModule({
  declarations: [AppLayoutComponent, HomePageComponent, DashboardComponent, SimpleAppLayoutComponent],
  imports: [CommonModule, RouterModule, CoreRoutingModule],
})
export class CoreModule {}
