import { NgModule } from '@angular/core';
import { Route, RouterModule } from '@angular/router';
import { UserProfileEditComponent } from './pages/user-profile-edit/user-profile-edit/user-profile-edit.component';
import { UserProfileComponent } from './pages/user-profile/user-profile.component';

const routes: Route[] = [
  { path: 'profile', component: UserProfileComponent },
  { path: 'profile-edit', component: UserProfileEditComponent },
];

@NgModule({
  imports: [RouterModule.forChild(routes)],
  exports: [RouterModule],
})
export class UserRoutingModule {}
