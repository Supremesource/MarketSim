import { type NextPage, type InferGetServerSidePropsType } from "next";
import axios from "axios";
// import Head from "next/head";
// import Link from "next/link";

const Home: NextPage<Props> = ({ data }) => {
  return (
    <main>
      <h1> Our first page </h1>
      <button className="btn btn-blue"> Do it! </button>
      <p> {JSON.stringify(data)} </p>
    </main>
  );
};

type Props = InferGetServerSidePropsType<typeof getServerSideProps>;

/* eslint-disable @typescript-eslint/no-unsafe-assignment */
export async function getServerSideProps(): Promise<{ props: {data: any} }> {

  // TODO: reduce request size somehow
  // ideas: 
  // 1. separate in 2 endpoints
  // 2. filter data on backend
  // 3. use pagination(kinda hard)

  // const r = await axios.get("http://127.0.0.1:3000/api/back/books");
  const r = await axios.get("http://127.0.0.1:8000/books");

  if (r.status != 200) {
    throw new Error(`Failed to fetch data: ${r.status}`);
  }

  const data = r.data
  return {
    props: {
      data,
    },
  };
}

export default Home;
