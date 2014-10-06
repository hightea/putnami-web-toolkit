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

import fr.putnami.pwt.core.theme.client.CssStyle;
import fr.putnami.pwt.core.widget.client.ButtonGroup;
import fr.putnami.pwt.core.widget.client.base.SimpleStyle;
import fr.putnami.pwt.core.widget.client.util.StyleUtils;

public class SocialBar extends ButtonGroup {

	private static final CssStyle STYLE_SOCIAL_BAR = new SimpleStyle("social-bar");

	public SocialBar() {
		StyleUtils.addStyle(this, SocialBar.STYLE_SOCIAL_BAR);
		this.add(new TwitterButton());
		this.add(new FacebookButton());
		this.add(new GooglePlusButton());
		this.add(new LinkedInButton());
		this.add(new GitHubButton());
	}

}
