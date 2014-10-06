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
