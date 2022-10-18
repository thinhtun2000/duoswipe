import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { AppLayoutComponent } from './core/layouts/app-layout/app-layout.component';
import { SimpleAppLayoutComponent } from './core/layouts/simple-app-layout/simple-app-layout.component';
import { DashboardComponent } from './core/pages/dashboard/dashboard.component';
import { ForbiddenComponent } from './core/pages/error-pages/forbidden/forbidden.component';
import { NotFoundComponent } from './core/pages/error-pages/not-found/not-found.component';
import { UnauthorizedComponent } from './core/pages/error-pages/unauthorized/unauthorized.component';
import { HomePageComponent } from './core/pages/home-page/home-page.component';
import { UserProfileComponent } from './user/pages/user-profile/user-profile.component';

const routes: Routes = [
  {
    path: '',
    pathMatch: 'full',
    redirectTo: 'home',
  },
  {
    path: 'auth',
    redirectTo: 'login',
  },
  {
    path: 'app',
    component: AppLayoutComponent,
    children: [
      { path: '', pathMatch: 'full', redirectTo: 'dashboard' },
      { path: 'dashboard', component: DashboardComponent },
      {
        path: 'user',
        loadChildren: () =>
          import('./user/user.module').then((m) => m.UserModule),
      },
    ],
  },
  {
    path: '',
    component: SimpleAppLayoutComponent,
    children: [
      {
        path: 'not-found',
        component: NotFoundComponent,
      },
      {
        path: 'forbidden',
        component: ForbiddenComponent,
      },
      {
        path: 'unauthorized',
        component: UnauthorizedComponent,
      },
    ],
  },
];

@NgModule({
  imports: [RouterModule.forRoot(routes)],
  exports: [RouterModule],
})
export class AppRoutingModule {}
