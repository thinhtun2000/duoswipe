import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { UserInfoCardComponent } from './components/user-info-card/user-info-card.component';
import { CoreModule } from '../core/core.module';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';

@NgModule({
  declarations: [UserInfoCardComponent],
  imports: [CommonModule, CoreModule, FormsModule, ReactiveFormsModule],
  exports: [UserInfoCardComponent],
})
export class SharedModule {}
