// @ts-check
// Note: type annotations allow type checking and IDEs autocompletion

const lightCodeTheme = require('prism-react-renderer/themes/github');
const darkCodeTheme = require('prism-react-renderer/themes/dracula');

/** @type {import('@docusaurus/types').Config} */
const config = {
  title: 'IOG Engineering',
  tagline: 'Advancing Technologie',
  url: 'https://engineering.iog.io',
  baseUrl: '/',
  onBrokenLinks: 'throw',
  onBrokenMarkdownLinks: 'warn',
  favicon: 'img/favicon.ico',
  organizationName: 'input-output-hk', // Usually your GitHub org/user name.
  projectName: 'engineering', // Usually your repo name.

  presets: [
    [
      'classic',
      /** @type {import('@docusaurus/preset-classic').Options} */
      ({
        docs: false,
        // {
        //   sidebarPath: require.resolve('./sidebars.js'),
        //   // Please change this to your repo.
        //   editUrl: 'https://github.com/input-output-hk/engineering/tree/master/',
        // },
        blog: {
          routeBasePath: '/',
          showReadingTime: true,
          // Please change this to your repo.
          editUrl: undefined,
            // 'https://github.com/input-output-hk/engineering/tree/master/',
          feedOptions: {
            type: 'all',
            description: 'IOG Engineering blog',
            language: 'en',
            copyright: `Copyright © ${new Date().getFullYear()} IOG, Inc.`
          }
        },
        theme: {
          customCss: require.resolve('./src/css/custom.css'),
        },
      }),
    ],
  ],

  themeConfig:
    /** @type {import('@docusaurus/preset-classic').ThemeConfig} */
    ({
      navbar: {
        title: 'Engineering',
        logo: {
          alt: 'IOG',
          src: 'img/iohk-logo.jpg',
        },
        items: [
          // {
          //   type: 'doc',
          //   docId: 'intro',
          //   position: 'left',
          //   label: 'Tutorial',
          // },
          {to: '/', label: 'Recent', position: 'left'},
          {to: '/tags', label: 'Tags', position: 'left'},
          {to: '/archive', label: 'Archive', position: 'left'},
          // {
          //   href: 'https://github.com/input-output-hk/',
          //   label: 'GitHub',
          //   position: 'right',
          // },
        ],
      },
      footer: {
        style: 'dark',
        links: [
          // {
          //   title: 'Docs',
          //   items: [
          //     {
          //       label: 'Tutorial',
          //       to: '/docs/intro',
          //     },
          //   ],
          // },
          {
            title: 'Community',
            items: [
              // {
              //   label: 'Cardano Stack Exchange',
              //   href: 'https://cardano.stackexchange.com/',
              // },
              // {
              //   label: 'Discord',
              //   href: 'https://discordapp.com/invite/docusaurus',
              // },
              {
                label: 'Twitter',
                href: 'https://twitter.com/iog_eng',
              },
            ],
          },
          {
            title: 'More',
            items: [
              {
                label: 'IOG Blog',
                to: 'https://iohk.io/en/blog',
              },
              {
                label: 'GitHub',
                href: 'https://github.com/input-output-hk/',
              },
            ],
          },
        ],
        copyright: `Copyright © ${new Date().getFullYear()} IOG Engineering, Inc. Built with Docusaurus.`,
      },
      prism: {
        theme: lightCodeTheme,
        darkTheme: darkCodeTheme,
      },
    }),
};

module.exports = config;
