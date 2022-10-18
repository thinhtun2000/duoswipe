import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { AppLayoutComponent } from './layouts/app-layout/app-layout.component';
import { RouterModule } from '@angular/router';
import { HttpClientModule } from '@angular/common/http';
import { HomePageComponent } from './pages/home-page/home-page.component';
import { DashboardComponent } from './pages/dashboard/dashboard.component';
import { CoreRoutingModule } from './core-routing.module';
import { SimpleAppLayoutComponent } from './layouts/simple-app-layout/simple-app-layout.component';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { UnauthorizedComponent } from './pages/error-pages/unauthorized/unauthorized.component';
import { ForbiddenComponent } from './pages/error-pages/forbidden/forbidden.component';
import { NotFoundComponent } from './pages/error-pages/not-found/not-found.component';
import { ErrorCardComponent } from './components/error-card/error-card/error-card.component';

@NgModule({
  declarations: [
    AppLayoutComponent,
    HomePageComponent,
    DashboardComponent,
    SimpleAppLayoutComponent,
    UnauthorizedComponent,
    ForbiddenComponent,
    NotFoundComponent,
    ErrorCardComponent,
  ],
  imports: [
    CommonModule,
    RouterModule,
    CoreRoutingModule,
    HttpClientModule,
    FormsModule,
    ReactiveFormsModule,
  ],
})
export class CoreModule {}
