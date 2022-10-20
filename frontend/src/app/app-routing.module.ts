import { NgModule } from '@angular/core';
import { RouterModule, Routes } from '@angular/router';
import { AppLayoutComponent } from './core/layouts/app-layout/app-layout.component';
import { DashboardComponent } from './core/pages/dashboard/dashboard.component';
import { HomePageComponent } from './core/pages/home-page/home-page.component';
import { UserProfileComponent } from './user/pages/user-profile/user-profile.component';
import { MatchPageComponent } from './core/pages/match-page/match-page.component';
import { SwipePageComponent } from './core/pages/swipe-page/swipe-page.component';

const routes: Routes = [
  {
    path: '',
    pathMatch: 'full',
    redirectTo: 'home',
  },
  {
    path: 'app',
    component: AppLayoutComponent,
    children: [
      { path: '', pathMatch: 'full', redirectTo: 'dashboard' },
      { path: 'dashboard', component: DashboardComponent },
      { path: 'matches', component: MatchPageComponent },
      { path: 'swipe', component: SwipePageComponent },
      {
        path: 'user',
        loadChildren: () =>
          import('./user/user.module').then((m) => m.UserModule),
      },
    ],
  },
];

@NgModule({
  imports: [RouterModule.forRoot(routes)],
  exports: [RouterModule],
})
export class AppRoutingModule {}
