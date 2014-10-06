/**
 * This file is part of pwt.
 *
 * pwt is free software: you can redistribute it and/or modify it under the terms of the GNU Lesser
 * General Public License as published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * pwt is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
 * implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
 * General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with pwt. If not,
 * see <http://www.gnu.org/licenses/>.
 */
package fr.putnami.pwt.doc.client.social.widget;

import fr.putnami.pwt.doc.client.social.base.AbstractSocialButton;

public class GitHubButton extends AbstractSocialButton {

	public GitHubButton() {
		this.setIconType(AbstractSocialButton.CONSTANT.githubIcon());
		this.setTitle(AbstractSocialButton.CONSTANT.githubPopover());
		this.setSocialUtlTemplate(AbstractSocialButton.CONSTANT.githubUrl());
		this.setAsPopup(false);
	}
}
